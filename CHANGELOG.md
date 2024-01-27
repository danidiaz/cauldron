# Revision history for cauldron

## 0.3.1.0 

* Now the `MissingDependencies` only includes keys with actual missing dependencies.

## 0.3.0.0 

* Add `cookNonEmpty` and `cookTree` for cooking hierarchies of 'Cauldron's.
* Rename `addLast` to `addOuter` and `addFirst` to `addInner`.
* Add a copy of the `Managed` type from ["managed"](https://hackage.haskell.org/package/managed).
* Change the nomenclature of the `pack-` related functions.
* Add the `Packer` type.
* Add `Fire` type to customize the handling of dependency cycles.

## 0.2.0.0 

* Decorators are no longer `Endo`s. They just take the decorated entity as a 
  regular parameter.

* Remove the applicative wrappers.

* Allow effectful constructors.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
