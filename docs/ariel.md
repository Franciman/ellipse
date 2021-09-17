% Ariel semantics

We want to prove that Ariel with letrec behaves as we expect, i.e. it behaves as the same program
encoded in a fix-based recursive language.

To this end we define two languages, one called `Ariel-rec` and one called `Ariel-fix`.

## Environments

In both cases we need the notion of environment. Having a nameless representation,
we want to be able to retrieve bindings by the index of the variable and we want
the possibility of adding a new binding for the `0` variable, shifting all other indices.

In the following `v` will represent a value of the language and `E` an environment.

We have therefore three functions defined for working with environments:

- $bind(v, E)$
: adds a new binding for the `0` variable, shifting all the other indices.

- $contains(n, E)$
: checks whether the environment contains a binding for the `n-th` variable

- $lookup(n, E)$
: returns the binding associated to the n-th variable.

Finally, we use the symbol $\emptyset$ for representing an empty environment.

## Ariel-fix

We can describe `Ariel-fix` as a nameless lambda calculus with a `fix` operator. Nothing fancy, indeed.

We can describe its terms as follows:

| $t := \textbf{n}$
|     $| \lambda . t$
|     $| \text{fix} t$
|     $| t \, t$

Values are described even more easily:

| $v := \lambda \langle E \rangle. t$
|     $| \text{fix} v$

The first case is a closure, i.e. a lambda abstraction packed with an environment $E$.

The second case represents a potential infinite loop :D

Finally the semantics is easily described by the following set of rules:

```nat-ded
(rule "\textit{lam}" "E \vdash \lambda . t \Downarrow \lambda \langle E \rangle . t")
```

```nat-ded
(rule "\textit{fix}" "E \vdash t \Downarrow v"
                     "E \vdash \text{fix} t \Downarrow \text{fix} v")
```

```nat-ded
(rule "\textit{var}" "contains(n, E)" "E \vdash n \Downarrow lookup(n, E)")
```

Beta reduction:

```nat-ded
(rule "$\beta$"
      "E \vdash t_1 \Downarrow \lambda \langle E' \rangle . t"
      "E \vdash t_2 \Downarrow v_2"
      "bind(v_2, E') \vdash t \Downarrow v"
      "E \vdash t_1 \, t_2 \Downarrow v")
```

Recursion:

In order to make things easier to read we use the following abbreviation:

$$\textbf{$v_1$} = \text{fix} (\lambda \langle E' \rangle . \lambda . t)$$

```nat-ded
(rule "$\mu$"
      "E \vdash t_1 \Downarrow \textbf{$v_1$}"
      "E \vdash t_2 \Downarrow v_2"
      "bind(v_2, bind(\textbf{$v_1$}, E')) \vdash t \Downarrow v"
      "E \vdash t_1 \, t_2 \Downarrow v")
```

## Ariel-rec

Now we define what is a nameless lambda calculus with a `letrec` statement allowing recursion

We can describe its terms as follows:

| $t := \textbf{n}$
|     $| \text{rec} \, \textbf{n}$
|     $| \lambda . t$
|     $| \text{letrec}\, t\, \text{in}\, t$
|     $| t \, t$

We have the letrect statement and a new type of variable, a `rec n` variable.

This variable is used to refer to the recursive value introduced by the `n-th` 
enclosing letrec statement (including in the letrect declaration itself, that's
what makes it special!).

Values are described even more easily:

| $v := \lambda \langle E \rangle. t$

We only have a closure, the news is that we have another environment for
recursive variables, associating to each of them a _term_ (not a _value_) and a
recursion environment to which it had access when being defined.


```nat-ded
(rule "\textit{lam}" "(E, R) \vdash \lambda . t \Downarrow \lambda \langle E \rangle . t")
```

```nat-ded
(rule "\textit{var}" "contains(n, E)" "(E, R) \vdash n \Downarrow lookup(n, E)")
```

And now a news, evaluation of a recursive variable

```nat-ded
(rule "\textit{rec-var}" "contains(n, R)"
                         "(E, R) \vdash lookup(n, R) \Downarrow v"
                         "(E, R) \vdash \text{rec}\, n \Downarrow v")
```
