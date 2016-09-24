# Plan for handling enum types

1. Add types to the AST: type names (maybe ignore built-ins for now),
   plus type declarations
2. Change the Type module so that there is no Nil, and TypeName
   includes a reference to the type it names. Add an EnumOption type
   that references the enum it came from.
3. Change TypeCheck to handle this -- always resolve types fully.
4. In Backends.Go.Convert, convert Enum types to Interfaces and both
   Struct and EnumOption values to structs.
