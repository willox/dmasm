use crate::compiler::*;

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

    fn resolve(&mut self) {
        match &mut self.var {
            Variable::Null => {
                self.var = Variable::Cache;
            }

            Variable::SetCache(lhs, rhs) => {
                match **rhs {
                    Variable::SetCache {..} => {
                        Self::resolve2(rhs);
                    }
                    _ => {
                        self.var = (**lhs).clone();
                    }
                }
            }

            _ => {}
        }
    }

    // TODO: dedupe
    fn resolve2(var: &mut Variable) {
        match var {
            Variable::SetCache(lhs, rhs) => {
                match **rhs {
                    Variable::SetCache {..} => {
                        Self::resolve2(rhs);
                    }
                    _ => {
                        *var = (**lhs).clone();
                    }
                }
            }

            _ => {}
        }
    }

    fn last_setcache_rhs(&mut self) -> Option<&mut Box<Variable>> {
        if let Variable::SetCache(_, rhs) = &mut self.var {
            if let Variable::SetCache { .. } = **rhs {
                return Self::last_setcache_rhs2(rhs.as_mut());
            }

            return Some(rhs);
        }

        None
    }

    // TODO: dedupe
    fn last_setcache_rhs2(var: &mut Variable) -> Option<&mut Box<Variable>> {
        if let Variable::SetCache(_, rhs) = var {
            if let Variable::SetCache { .. } = **rhs {
                return Self::last_setcache_rhs2(rhs.as_mut());
            }

            return Some(rhs);
        }

        None
    }

    pub fn append(&mut self, field: DMString) {
        if let Some(rhs) = self.last_setcache_rhs() {
            **rhs = Variable::SetCache(Box::new(Variable::Field(field)), Box::new(Variable::Null));
            return;
        }

        // This is the first SetCache - discard the current var (which should be null)
        assert!(self.var == Variable::Null);
        self.var = Variable::SetCache(Box::new(Variable::Field(field)), Box::new(Variable::Null));
    }

    pub fn get(mut self) -> Option<Variable> {
        self.resolve();
        Some(self.var)
    }

    pub fn get_field(mut self, field: DMString) -> Variable {
        if let Some(rhs) = self.last_setcache_rhs() {
            **rhs = Variable::Field(field);
            return self.var;
        }

        Variable::Field(field)
    }

    pub fn get_initial_field(mut self, field: DMString) -> Variable {
        if let Some(rhs) = self.last_setcache_rhs() {
            **rhs = Variable::Initial(Box::new(Variable::Field(field)));
            return self.var;
        }

        Variable::Initial(Box::new(Variable::Field(field)))
    }

    pub fn get_dynamic_proc(mut self, proc: DMString) -> Variable {
        if let Some(rhs) = self.last_setcache_rhs() {
            **rhs = Variable::DynamicProc(proc);
            return self.var;
        }

        Variable::DynamicProc(proc)
    }
}
