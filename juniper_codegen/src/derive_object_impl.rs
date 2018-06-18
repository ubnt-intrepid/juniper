use proc_macro::TokenStream;
use std::collections::HashMap;
use syn::{
    parse, AngleBracketedGenericArguments, Attribute, Binding, FnArg, GenericArgument,
    ImplItem, ImplItemMethod, Item, ItemImpl, Lit, Meta, NestedMeta, PathArguments, ReturnType,
    Type, TypePath, TypeReference,
};

fn get_attr_map(attr: &Attribute) -> Option<(String, HashMap<String, String>)> {
    let meta = attr.interpret_meta();

    let meta_list = match meta {
        Some(Meta::List(ref meta_list)) => meta_list,
        _ => return None,
    };

    let ident = meta_list.ident.to_string();

    let mut attr_map = HashMap::new();

    for meta in meta_list.nested.iter() {
        let value = match meta {
            NestedMeta::Meta(Meta::NameValue(ref value)) => value,
            _ => continue,
        };

        let name = value.ident.to_string();

        let ident = match value.lit {
            Lit::Str(ref string) => string.value(),
            _ => continue,
        };

        attr_map.insert(name, ident);
    }

    Some((ident, attr_map))
}

pub fn impl_gql_object(ast: Item) -> TokenStream {
    let ItemImpl {
        attrs,
        defaultness,
        unsafety,
        impl_token,
        generics,
        trait_,
        mut self_ty,
        mut items,
        brace_token,
    } = if let Item::Impl(imp) = ast {
        imp
    } else {
        panic!("#[gql_object] Can only be applied to impl blocks");
    };

    let (name, context) = if let Type::Path(TypePath { ref mut path, .. }) = *self_ty {
        let context = {
            let mut segment = path
                .segments
                .iter_mut()
                .last()
                .expect("Paths can't have 0 segments");
            let context = if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                                                                   ref args,
                                                                   ..
                                                               }) = segment.arguments
                {
                    args.iter().filter_map(|arg| {
                        if let GenericArgument::Binding(Binding { ref ident, ref ty, .. }) = arg {
                            if ident == "Context" {
                                return Some(ty)
                            }
                        }
                        None
                    })
                        .next()
                        .map(|ty| ty.clone())
                        .expect("#[gql_object] requires context to be specified with `impl MyType<Context=MyContext>`")
                } else {
                panic!("#[gql_object] requires context to be specified with `impl MyType<Context=MyContext>`");
            };
            segment.arguments = PathArguments::None;
            context
        };
        (path.clone(), context)
    } else {
        panic!("#[gql_object] only works with struct impls");
    };

    let description = attrs
        .iter()
        .filter_map(get_attr_map)
        .find(|(name, _)| name == "graphql")
        .map(|(_, map)| map.get("description").map(|i| i.clone()));

    let parsed: TypeReference = parse(quote!(&Executor<#context>).into()).unwrap();

    let mut fns = Vec::new();

    for item in &mut items {
        match item {
            ImplItem::Const(..) => panic!("Unexpected const item"),
            ImplItem::Macro(..) => panic!("Unexpected macro item"),
            ImplItem::Verbatim(..) => panic!("Unexpected verbatim item"),
            ImplItem::Type(..) => panic!("Unexpected type item"),
            ImplItem::Method(ImplItemMethod {
                                 sig, ref mut attrs, ..
                             }) => {
                let (description, deprecated) = if let Some((_, map)) = attrs
                    .iter()
                    .filter_map(get_attr_map)
                    .find(|(name, _)| name == "graphql")
                    {
                        (
                            map.get("description").map(|i| i.clone()),
                            map.get("deprecated").map(|i| i.clone()),
                        )
                    } else {
                    (None, None)
                };

                attrs.clear();

                match sig.decl.inputs[0] {
                    FnArg::Captured(ref arg) => match arg.ty {
                        Type::Reference(ref ty) if ty == &parsed => {}
                        _ => continue,
                    },
                    _ => continue,
                }

                let ret: Type = match sig.decl.output {
                    ReturnType::Type(_, ref ret) => (**ret).clone(),
                    _ => continue,
                };

                let mut fn_args = Vec::new();

                for arg in sig.decl.inputs.iter().skip(1) {
                    if let FnArg::Captured(arg) = arg {
                        fn_args.push((arg.pat.clone(), arg.ty.clone()));
                    } else {
                        panic!("invalid arg {:?}", stringify!(arg));
                    }
                }

                fns.push((sig.ident.clone(), fn_args, ret, description, deprecated));
            }
        }
    }

    let item = Item::Impl(ItemImpl {
        attrs: Vec::new(),
        defaultness,
        unsafety,
        impl_token,
        generics,
        trait_,
        self_ty,
        items,
        brace_token,
    });

    let exec_fns = fns.iter().map(|(name, args, _, _, _)| {
        let get_args = args.iter().map(|(arg_name, arg_type)| {
            quote! {
              let #arg_name: #arg_type = args.get(&juniper::to_camel_case(stringify!(#arg_name))).expect("Argument missing - validation must have failed");
            }
        });

        let arg_names = args.iter().map(|(name, _)| name);

        quote! {
          if field == &juniper::to_camel_case(stringify!(#name)) {
            #(#get_args)*

            let result = Self::#name(&executor, #( #arg_names ),*);
            return (juniper::IntoResolvable::into(result, executor.context())).and_then(|res|
                match res {
                  Some((ctx, r)) => executor.replaced_context(ctx).resolve_with_ctx(&(), &r),
                  None => Ok(juniper::Value::null()),
                });
          }
        }
    });

    let register_fns = fns
        .iter()
        .map(|(name, args, ret, description, deprecation)| {
            let args = args.iter().map(|(arg_name, arg_type)| {
                quote! {
                  .argument(registry.arg::<#arg_type>(&juniper::to_camel_case(stringify!(#arg_name)), info))
                }
            });

            let description = match description {
                Some(description) => quote!(.description(#description)),
                None => quote!(),
            };

            let deprecation = match deprecation {
                Some(deprecation) => quote!(.deprecation(#deprecation)),
                None => quote!(),
            };

            quote! {
              fields.push(
                registry
                    .field_convert::<#ret, _, Self::Context>(&juniper::to_camel_case(stringify!(#name)), info)
                    #(#args)*
                    #description
                    #deprecation
              );
            }
        });

    let description = match description {
        Some(description) => quote!(mt = mt.description(#description);),
        None => quote!(),
    };

    let gql_impl = quote! {
      impl juniper::GraphQLType for #name {
        type Context = #context;
        type TypeInfo = ();

        fn name(_: &Self::TypeInfo) -> Option<&str> {
          Some(stringify!(#name))
        }

        #[allow(unused_assignments)]
        #[allow(unused_mut)]
        fn meta<'r>(info: &Self::TypeInfo, registry: &mut juniper::Registry<'r>) -> juniper::meta::MetaType<'r> {
          let mut fields = Vec::new();
          let mut interfaces: Option<Vec<Type>> = None;
          #(#register_fns)*
          let mut mt = registry.build_object_type::<#name>(info, &fields);

          #description

          if let Some(interfaces) = interfaces {
            mt = mt.interfaces(&interfaces);
          }

          mt.into_meta()
        }

        #[allow(unused_variables)]
        #[allow(unused_mut)]
        fn resolve_field(
          &self,
          _info: &(),
          field: &str,
          args: &juniper::Arguments,
          executor: &juniper::Executor<Self::Context>,
        ) -> ExecutionResult {
          #(#exec_fns)*

          panic!("Field {} not found on type {}", field, "Mutation");
        }

        fn concrete_type_name(&self, _: &Self::Context, _: &Self::TypeInfo) -> String {
          stringify!(#name).to_string()
        }
      }
    };

    let res = quote! {
      #item
      #gql_impl
    };

    res.into()
}
