# sd-mech-model

This is a model for the "mechanism" of [Snowdrift][1]. It is written in
[Haskell][2] and licensed under the
[GNU Affero General Public License, version 3 or later][3] (AGPL). You
can read a copy of the AGPL in the [LICENSE](LICENSE) file.

**What is Snowdrift?**

It's a project to cooperatively and sustainably fund
[freely-licensed works][4]. For more information, see the
[How it works page][5].

**What is the mechanism?**

The mechanism is the library in Snowdrift that implements the
[Accounting information system][6] in Snowdrift. You can read more about
it on the [Snowdrift wiki][w-mech].

## Building

To build this software, you need [Git][git-install] and
[the Haskell Stack][hs-stack].

    git clone https://github.com/pharpend/sd-mech-model.git
    cd sd-mech-model
    stack setup
    stack build
    
That will just compile the library, which isn't very interesting on its
own. You can run the test suite by running

     stack test

and you'll get a nice colorful output explaining that everything is
peachy (or maybe not).

## Contacting the authors

Bug reports and feature requests can go to the
[GitHub issue tracker][gh-issues].

* Author of this package, Peter Harpending
    + email: `peter@harpending.org`
    + IRC: `pharpend` on FreeNode
* Snowdrift project
    + homepage: <https://snowdrift.coop>
    + IRC: `#snowdrift` on FreeNode
    + development mailing list: `dev@lists.snowdrift.coop`

[1]: https://snowdrift.coop
[2]: https://www.haskell.org/
[3]: https://gnu.org/licenses/agpl
[4]: https://en.wikipedia.org/wiki/Free_license
[5]: https://snowdrift.coop/how-it-works
[6]: https://en.wikipedia.org/wiki/Accounting_information_system
[gh-issues]: https://github.com/pharpend/sd-mech-model/issues
[git-install]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
[hs-stack]: http://docs.haskellstack.org/en/stable/README/
[w-mech]: https://snowdrift.coop/p/snowdrift/w/en/mechanism
