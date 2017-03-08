# coq-log-analysis

[![Join the chat at https://gitter.im/coq-log-analysis/Lobby](https://badges.gitter.im/coq-log-analysis/Lobby.svg)](https://gitter.im/coq-log-analysis/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Parse Coq log files and plot resolution trees

This scrpit helps you to visualize Coq proof search trees for teactics
like `setoid_rewrite` and `refine`. Fist, create a log file:

    Set Typeclasses Debug .
    Set Typeclasses Debug Verbosity 2.
    Set Printing All.
    Redirect "log.txt" setoid_rewrite lemma1.


Then process using this script:

    parselog -d -f log.txt.out -o log.dot
    dot -Tpdf log.dot -olog.pdf


This will produce `log.pdf`. You can also generate a file in any
dormat supported by `dot(1)`.

You can see [sample PDF](samples/samplepdf.pdf).

Author: Vadim Zaliva lord@crocodile.org
