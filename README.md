# cauldron

Put your recipes in the cauldron, make it boil, and get a tasty bean in return! 

## Rules of dependencies

- An undecorated bean depends on its own dependencies (duh!)
- A bean decorator depends on its own dependencies (duh!) and either the undecorated bean (for the first decorator) or the previous bean decorator.
- The fully built bean depends either on the undecorated bean (if there are no decorators) or the last bean decorator.
- "Registrations" depend on all the components (beans or decorators) that produce them. 
