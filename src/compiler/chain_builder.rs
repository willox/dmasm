use crate::operands::{DMString, Variable};

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
            **rhs = Variable::SetCache(Box::new(Variable::Field(field)), Box::new(Variable::Null));
            return;
        }

        // This is the first SetCache - discard the current var (which should be null)
        assert!(self.var == Variable::Null);
        self.var = Variable::SetCache(Box::new(Variable::Field(field)), Box::new(Variable::Null));
    }

    pub fn get(mut self) -> Variable {
        Self::resolve(&mut self.var);
        self.var
    }

    pub fn get_field(mut self, field: DMString) -> Variable {
        if let Some(rhs) = Self::last_setcache_rhs(&mut self.var) {
            **rhs = Variable::Field(field);
            return self.var;
        }

        Variable::Field(field)
    }

    pub fn get_initial_field(mut self, field: DMString) -> Variable {
        if let Some(rhs) = Self::last_setcache_rhs(&mut self.var) {
            **rhs = Variable::Initial(Box::new(Variable::Field(field)));
            return self.var;
        }

        Variable::Initial(Box::new(Variable::Field(field)))
    }
}
