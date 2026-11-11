use crate::operands::{DMString, GlobalVar, Variable};

/// Marks a chain element that is a static var: a global, read through the
/// cache chain like a field.
pub const STATIC_MARKER: &[u8] = b"\0static:";

fn field_var(field: DMString) -> Variable {
    match field.0.strip_prefix(STATIC_MARKER) {
        // "name\0/declaring/type"
        Some(rest) => {
            let mut parts = rest.splitn(2, |&b| b == 0);
            let name = parts.next().unwrap_or_default();
            let scope = parts
                .next()
                .map(|s| String::from_utf8_lossy(s).into_owned());
            Variable::Global(GlobalVar::named(name, scope))
        }
        None => Variable::Field(field),
    }
}

#[derive(Debug, PartialEq)]
pub struct ChainBuilder {
    var: Variable,
}

impl ChainBuilder {
    pub fn begin(base: Variable) -> Self {
        let var = match base {
            Variable::Cache => {
                // We're already in the cache so there's nothing to do here
                // This null gets replaced later
                Variable::Null
            }

            other => Variable::SetCache(Box::new(other), Box::new(Variable::Null)),
        };

        Self { var }
    }

    fn resolve(var: &mut Variable) {
        match var {
            Variable::Null => {
                *var = Variable::Cache;
            }

            Variable::SetCache(lhs, rhs) => match **rhs {
                Variable::SetCache { .. } => {
                    Self::resolve(rhs);
                }
                _ => {
                    *var = (**lhs).clone();
                }
            },

            _ => {}
        }
    }

    fn last_setcache_rhs(var: &mut Variable) -> Option<&mut Box<Variable>> {
        if let Variable::SetCache(_, rhs) = var {
            if let Variable::SetCache { .. } = **rhs {
                return Self::last_setcache_rhs(rhs.as_mut());
            }

            return Some(rhs);
        }

        None
    }

    pub fn append(&mut self, field: DMString) {
        if let Some(rhs) = Self::last_setcache_rhs(&mut self.var) {
            **rhs = Variable::SetCache(Box::new(field_var(field)), Box::new(Variable::Null));
            return;
        }

        // This is the first SetCache - discard the current var (which should be null)
        assert!(self.var == Variable::Null);
        self.var = Variable::SetCache(Box::new(field_var(field)), Box::new(Variable::Null));
    }

    pub fn get(mut self) -> Variable {
        Self::resolve(&mut self.var);
        self.var
    }

    pub fn get_field(mut self, field: DMString) -> Variable {
        if let Some(rhs) = Self::last_setcache_rhs(&mut self.var) {
            **rhs = field_var(field);
            return self.var;
        }

        field_var(field)
    }

    /// The chain with `leaf` evaluated against the final cache, e.g. a
    /// `dynamic_proc` to call.
    pub fn get_leaf(mut self, leaf: Variable) -> Variable {
        if let Some(rhs) = Self::last_setcache_rhs(&mut self.var) {
            **rhs = leaf;
            return self.var;
        }

        leaf
    }

    pub fn get_initial_field(mut self, field: DMString) -> Variable {
        if let Some(rhs) = Self::last_setcache_rhs(&mut self.var) {
            **rhs = Variable::Initial(Box::new(Variable::Field(field)));
            return self.var;
        }

        Variable::Initial(Box::new(Variable::Field(field)))
    }
}
