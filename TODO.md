- deco indexing should begin at 0.
- allow cooking a whole tree of cauldrons.
    - "check no double duty beans" could be easily (?) generalized to a tree.
        - start by this.
        - what to do about monoidal accumulators? 
            - what if a component in 0 depends on a monodial acc defined in 1?
            - currently, accums are NEVER considered "missing dependencies".

    - idea:
    - calculate the accumulators upwards.
    - then calculate the "missing deps"
        Tree (Map TypeRep (SomeBean m)) -> Tree (Map TypeRep (SomeBean m, Map TypeRep (Maybe IndexList)))

