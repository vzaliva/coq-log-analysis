# coq-log-analysis

Parse Coq log files and plot resolution trees

This scrpit helps you to visualize Coq search trees for teactics like `setoid_rewrite` and refine. Fist create a log file:

    Set Typeclasses Debug .
    Set Typeclasses Debug Verbosity 2.
    Set Printing All.
    Set Debug Auto.
    Set Debug Eauto.
    Redirect "log.txt" setoid_rewrite lemma1.


Then process using this script:

    parselog -d -f log.txt.out -o log.dot
    dot -Tpdf log.dot -olog.pdf


This will produce `log.pdf`. You can also generate a file in any dormat supported by `dot(1)`.

You can see [sample PDF](samples/samplepdf.pdf).

Author: Vadim Zaliva lord@crocodile.org
