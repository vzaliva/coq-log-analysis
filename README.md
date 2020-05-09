# coq-log-analysis

Parse Coq log files and plot resolution trees

This scrpit helps you to visualize Coq proof typeclass search trees
for tactics like `setoid_rewrite` and `refine`. Fist, create a log
file:

    Set Typeclasses Debug .
    Set Typeclasses Debug Verbosity 2.
    Set Printing All.
    Redirect "log.txt" setoid_rewrite lemma1.

Then process using the following commands:

    parselog -d -f log.txt.out -o log.dot
    dot -Tpdf log.dot -olog.pdf

This will produce `log.pdf`. You can also generate a file in any
dormat supported by `dot(1)`.

You can see [sample PDF](samples/samplepdf.pdf).

Author: Vadim Zaliva lord@crocodile.org
