# Revision history for cauldron

## 0.2.0.0 

* Remove the possibility of self-reference.

* Decorators are no longer `Endo`s. They just take the decorated entity as a 
  regular parameter.

* `cook` now specifies a target bean. Only that bean and its transitive
  dependencies are built.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
