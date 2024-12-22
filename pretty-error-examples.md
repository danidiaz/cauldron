```
A constructor for a value of type G is missing the following dependencies:
- E
- F
	CallStack (from HasCallStack):
	  val, called at app/Main.hs:206:26 in cauldron-0.4.0.0-inplace-cauldron-example-wiring:Main
```

```
The following beans work both as primary beans and secondary beans:
- Inspector is a secondary bean here:
	CallStack (from HasCallStack):
	  val, called at app/Main.hs:212:25 in cauldron-0.4.0.0-inplace-cauldron-example-wiring:Main
  and a primary bean here:
	CallStack (from HasCallStack):
	  recipe, called at app/Main.hs:201:13 in cauldron-0.4.0.0-inplace-cauldron-example-wiring:Main
- Initializer is a secondary bean here:
	CallStack (from HasCallStack):
	  val, called at app/Main.hs:219:25 in cauldron-0.4.0.0-inplace-cauldron-example-wiring:Main
  and a primary bean here:
	CallStack (from HasCallStack):
	  recipe, called at app/Main.hs:202:13 in cauldron-0.4.0.0-inplace-cauldron-example-wiring:Main
```

```
Forbidden dependency cycle between bean construction steps:
- Complete bean F
	CallStack (from HasCallStack):
	  recipe, called at app/Main.hs:202:13 in cauldron-0.4.0.0-inplace-cauldron-example-wiring:Main
- Bare bean F
	CallStack (from HasCallStack):
	  val, called at app/Main.hs:202:25 in cauldron-0.4.0.0-inplace-cauldron-example-wiring:Main
- Complete bean G
	CallStack (from HasCallStack):
	  recipe, called at app/Main.hs:203:13 in cauldron-0.4.0.0-inplace-cauldron-example-wiring:Main
- Decorator 0 for bean G
	CallStack (from HasCallStack):
	  val, called at app/Main.hs:208:25 in cauldron-0.4.0.0-inplace-cauldron-example-wiring:Main
- Bare bean G
	CallStack (from HasCallStack):
	  val, called at app/Main.hs:205:26 in cauldron-0.4.0.0-inplace-cauldron-example-wiring:Main
```

