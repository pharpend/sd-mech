# sd-mech

This is an implementation of the [Snowdrift mechanism][sd-mech]. It is
written in [Haskell][hs] and licensed under the
[GNU Affero General Public License, version 3 or later][agpl]
(AGPL). You can read a copy of the AGPL in the [LICENSE][l] file.

**What is Snowdrift?**

It's a project to cooperatively and sustainably fund
[freely-licensed works][w-free]. For more information, see the
[How it works page][sd-how].

The mechanism is described in great detail on
[the Snowdrift wiki][sd-mech].

## Building

To build this software, you need [Git][git-install] and
[the Haskell Stack][hs-stack]. You also need to have [PostgreSQL][pg]
installed; however, you do not need to perform any additional setup
beyond installing it. (I think).

    git clone https://gitlab.com/pharpend/sd-mech.git
    cd sd-mech
    stack setup
    stack build
    
That will just compile the library, which isn't very interesting on its
own. 

There's a test suite, which you can run. Before you do so, you need to
do some postgres voodoo. Luckily, we have it automated

    ./sdms.hs init

If you get any weird database errors in the future, try running
`./sdms.hs reset`. If that doesn't fix it, it's likely unfixable. You
can run the test suite by running

     stack test

and you'll get a nice colorful output explaining that everything is
peachy (or ~~maybe~~ probably not).

### A note on the test suite

There are actually two of them: one called `pure` and another called
`impure`. In Haskell, code is conceptually split into "pure" code (code
that does not have side effects) and "impure" code (code that can
actually do stuff). As you might have guessed, one suite tests the pure
code, and the other tests the impure code.

## Contacting the authors

Bug reports and feature requests can go to the
[GitLab issue tracker][gl-issues].

* Author of this package, Peter Harpending
    + email: `peter@harpending.org`
    + IRC: `pharpend` on FreeNode
* Snowdrift project
    + homepage: <https://snowdrift.coop>
    + IRC: `#snowdrift` on FreeNode
    + development mailing list: `dev@lists.snowdrift.coop`

[agpl]: https://gnu.org/licenses/agpl
[gl-issues]: https://gitlab.com/pharpend/sd-mech-model/issues
[git-install]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
[hs]: https://www.haskell.org/
[hs-stack]: http://docs.haskellstack.org/en/stable/README/
[l]: LICENSE
[pg]: https://wiki.postgresql.org/wiki/Detailed_installation_guides
[sd-how]: https://snowdrift.coop/how-it-works
[sd-mech]: https://snowdrift.coop/p/snowdrift/w/en/mechanism
[w-free]: https://en.wikipedia.org/wiki/Free_license
