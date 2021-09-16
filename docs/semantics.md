% Semantics of Ellipse


We define a `big step` semantics for our language using environments.

Therefore we need a definition of environments, they are map-like object with the possibility of finding the associated value to a variable name.

Therefore we have two operations:

```
x = v, E
```

which adds a new binding to the environment E, creating a new environment (and possibly overloading the old value for x).

```
lookup(x, E)
```

a partial function which returns the value associated to x, if any.

Finally we have a predicate telling us whether a variable is bound in the environment:

```
bound(x, E)
```


The core language's grammar is given by

| $t := x$
|       $|\, \lambda x. t$
|       $|\, t \, t$
|       $|\, fix\, t$

Values are taken to be, observe that lambda functions are paired with an environment, they form the so called `closures`.

| $v := (E, \lambda x. t)$
|       $|\, fix\, v$

Observe that `fix v` is a value, this is a bit strange, because it is an infinite loop,
but in order to define a big step semantics for call by value (more amenable to fast implementations),
it seems inevitable to add it as a value and step by step unwrap it while it is applied
to actual arguments. Because if we tried to evaluated it upfront, we would get
non-termination in all cases, which is not the correct semantics we want to capture.

Everything is ready for the big step semantics to be defined, we inductively define a ternary relation $E \vdash t \Downarrow v$.

Each value evaluates to itself in a big step of evaluation.

```nat-ded
(axiom "value" "E \vdash v \Downarrow v")
```

When variables are in scope, we look up their value in the environment:

```nat-ded
(rule "var-eval" "bound(x, E)" "E \vdash x \Downarrow lookup(x, E)")
```

When we encounter a `fix t`, we want to fully evaluate its argument, easy peasy:

```nat-ded
(rule "fix-eval" "E \vdash t \Downarrow v" "E \vdash fix\, t \Downarrow fix\, v")
```

Now let us define how beta-reduction works:

```nat-ded
(rule "beta"
      "E \vdash t_1 \Downarrow (E', \lambda x. t)" 
      "E \vdash t_2 \Downarrow v_2"
      "x = v_2, E \vdash t \Downarrow v"
      "E \vdash t_1\, t_2 \Downarrow v")
```

Finally we need to define what happens when the left-hand side of function application
is the fix combinator. When it is applied to something, we want to unwrap it slightly
and see what happens.

```nat-ded
(rule "recur"
      "E \vdash t_1 \Downarrow fix\, (E', \lambda x. \lambda y. t)"
      "E \vdash t_2 \Downarrow v_2"
      "y = v_2, x = fix\, (E', \lambda x. t), E' \vdash t \Downarrow v"
      "E \vdash t_1\, t_2 \Downarrow v")
```

and here is the notable part, here we unwrap fix a bit, to see what happens when
making a recursive call, i.e. calling the param of fix, with another fix as argument
and $v_2$ as second argument, if this terminates to a value, then it is the result of
the computation.
