Maintaining PHP Mode
====================

By Eric James Michael Ritz



Introduction
------------

This impetus for this document was [this dicussion on GitHub][gh356],
specifically [some questions by Glynn Forrest][gf-questions].  He
wanted to know about my typical workflow in the past six years I have
maintained PHP Mode (at this time of writing).  So I thought this
would be a good way to not only answer his questions but provide
hopefully useful information to future maintainers.



Pull Requests
-------------

I have spent more time handling pull requests (PRs) from many
contributors than I have implementing my own code.  Managing PRs is,
in my opinion, the most important job for a maintainer to get right,
and so I want to describe my process in detail.

### Step One: Fetching the Pull Request ###

I like to use [hub][], a wrapper around the Git command-line
interface; although unlike the official site suggests, I do *not*
alias `hub` to `git`.  Anyway, it is helpful for starting the
process of reviewing and merging PRs.

Let's say I receive a pull request from the developer Foobar,
e.g. from a branches at `https://github.com/Foobar/php-mode`.  The
first thing I do is this:

``` shell
$ cd $PATH_TO_PHP_MODE
$ hub fetch Foobar
```

This will add Foobar's repository as a "remote" (in the `git-remote`
sense of the term) and will fetch *all* of his or her branches.  Now I
have all of those developers branches locally.  And if he or she ever
updates any or creates new ones, a simple `git fetch --prune Foobar`
will grab them all.

**Note:** `--prune` is not necessary but worth using every now and
then to cull out-dated and deleted branches.

### Checking Out the Pull Request ###

Let's say Foobar's branch in the pull request is named `fixes-stuff`.
First of all, politely reprimand that developer for using a very vague
and uninformative branch name.  Do not make such a big deal out of it
they you will not accept the patch until Foobar changes the name, but
make it clear that future pull requests should have better names like
`fix-indentation-on-closures` or `improve-imenu-for-namespaces`, etc.

The next step is to checkout the pull request.

``` shell
$ git checkout Foobar/fixes-stuff
```

This creates a "detatched HEAD" in Git, and it will give you a
seemingly scary message, but in reality this is completely fine and
you shouldn't worry about it.

### Test the Changes ###

Open `php-mode.el` and make sure it byte-compiles without error.  If
it does not, provide Foobar with the entire error output, along with
any suggestions for fixes you may immediately notice.  If the error is
trivial, like a simple typo, then don't hesitate to fix it yourself
instead of making everyone wait for one typo fix.

Then open `php-mode-tests.el` and run `ert` to make sure all tests
pass.  Again, if any do not, report that to the pull request author
with as much information as possible.

### Merging ###

#### Single Commits ####

If everything looks good then it's time to merge the pull request into
the `master` branch.  There are two ways to do this depending on the
situation, and in neither case should you use GitHub's interface.

Seriously.  *Never use it.*  I cannot overstress that.

The reason?  GitHub creates a completely unnecessary merge commit for
pull requests which themselves contain only a single commit.  You end
up with this structure in the repository history:

    Merge Commit
	   |    \
	   |     \
	   |      \
	   |  Foobar/fixes-stuff
	   |      /
	   |     /
	   |    /
	   |   /
	   |  /
	 master

That merge commit conveys absolutely zero useful information and is
nothing but noise and clutter.  If the pull request is a single
commit, put it directly on top of `master`.  The most common way I do
this is by cherry-picking:

``` shell
$ git checkout master
$ git cherry-pick -s Foobar/fixes-stuff
```

**Note:** The `-s` is not necessary.  It adds a "Signed-off-by" line
to the patche's commit message.  I like to do this as an indication
that I personally reviewed and tested the patch.  If you do this, then
keep in mind it will change the SHA-1 hash to something different from
the original pull request commit.  In which case you should *always*
copy-paste the full SHA-1 hash for the cherry-picked commit when
closing the pull request, so that the original author can see that you
merged his or her patch and delete the local branch accordingly.

#### Multiple Commits ####

If the pull request has multiple commits then *always* create a merge
commit.  Do not rebase them on top of `master` or I will find you and
stab you to death.  Heh, just kidding---would use a rifle instead.

By default Git will try to create a "fast-forward merge", which will
not create a merge commit.  You do not want this.  So make sure to
explicitly create a merge commit like so:

``` shell
$ git checkout master
$ git merge --no-ff --edit
```

Using the `--edit` option allows you to edit the message of the merge
commit.  I use it to add a "Signed-off-by" line, indicating I reviewed
*every* commit in the branch instead of going through and adding that
line to every individual commit.

Why the emphasis on creating merge commits for pull request branches
that have multiple commits?  It is so that you can `git revert -m`
those merges in the future, to undo the entire PR branch if
necessary.  Although pray to whatever higher powers you believe in
that you'll never have to do this, because it can become messy in a
hurry.



Future Topics to Write
----------------------

- Using the test suite with `git bisect` to find bugs.
- Writing unit tests.
- Standards for commit messages.
- Keeping the contributor list up to date.
- Rewriting commit messages.
- Meta-info in messages, like references to GitHub issues.



[gh356]: https://github.com/ejmr/php-mode/issues/365 "New Co-maintainer to Replace Me"
[gf-questions]: https://github.com/ejmr/php-mode/issues/365#issuecomment-317711820
[hub]: https://hub.github.com/
