
# Table of Contents

1.  [Recursive Formatter](#orgedcb2e3)
2.  [Potential new syntax <code>[0/1]</code>](#org4e6811f)
    1.  [Switch Case](#org51eca04)
3.  [Modules <code>[0/1]</code>](#orgfe986eb)


<a id="orgedcb2e3"></a>

# Recursive Formatter

This project tries to formalize the Nytryx programming language

    stack build
    stack install

Add to the path since stack installs here

    # STACK
    export PATH="$HOME/.local/bin:$PATH"

Now you can use it anywhere in the system!

    nyf help


<a id="org4e6811f"></a>

# TODO Potential new syntax <code>[0/1]</code>


<a id="org51eca04"></a>

## TODO Switch Case

This new syntax [Could be used here instead](file:///home/l/xos/projects/c/nytrix/src/std/core/reflect.ny) it reoves the noise and leave
a clear mapping between memory and consequence.

    define x = 3; Global variable
    
    match tag {
        0x4c495354
        0x44494354
        if (x > 3); <- Guard (Do it later)
        0x5345545f
        0x5455504c -> load64(x + 8)
        0x44494354
        0x5345545f
        0x5455504c -> load64(x + 8)
        otherwise
          -> runsomething()
    }


<a id="orgfe986eb"></a>

# TODO Modules <code>[0/1]</code>

-   [ ] Modules must be [functors](https://ncatlab.org/nlab/show/functor) The same as the [Ocaml module system](https://ocaml.org/manual/5.4/moduleexamples.html)

![img](./etc/functor.jpg)

