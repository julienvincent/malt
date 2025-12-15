# Malt

Here lies an experiment to see if it's possible to combine Malli schemas with Clojure protocols.

I want the ability to define somewhat typed interfaces in Clojure that don't suffer from bit-rot (which the
`:malli/schema` metadata annotation suffers from) and that don't require adopting a complete typed solution for Clojure.

I personally use Malli for validation/parsing which means my code is already thoroughly littered with Malli schemas. I
do not want to adopt some other syntax or schema just for the sake of making typed interfaces.

The experimentational question is thus:

1. Can I make a `defprotocol` macro which accepts Malli schemas as parameters, which;
2. Produces a native Clojure protocol, and;
3. Adds runtime input/output validation using the schemas on the protocol, and;
4. My editor understand the syntax well enough to support protocol-level operations like go-to-implementations?

The answer? A resounding "yes"!
