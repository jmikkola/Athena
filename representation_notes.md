# Notes on internal representations

Currently, there is only one internal representation, the parsed AST.

This works well for extremely simple programs, but starts to cause
problems as the feature set becomes more complex.

Proposed internal representations:

## AST

Keep the AST structure as-is.

## Typed structure

This structure will be similar from the AST, but differ in two keys
ways: each node will (when appropriate) be tagged with the complete
type (the concrete type, or in the case of generics, as close to
concrete as the type can be made), and may combine things with
different syntactical representations into a single form.

Tagging each node with the type may not be the most efficient thing in
the world, but this project is a long way from the point where it
needs to care about compiler efficiency.

This is the structure that will be given to the various backends to
emit. Some may choose to convert this to another internal
representation that is easier to emit.
