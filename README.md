# amsthm-to-anki

[![Build Status](https://travis-ci.org/siddharthist/amsthm-to-anki.svg?branch=master)](https://travis-ci.org/siddharthist/amsthm-to-anki)

This is a simple program to transform LaTeX math notes (written with
the [`amsthm` package][amsthm]) into [Anki][anki] notecards. It converts things
like definitions, theorems, lemmas, corrolaries, and equations into the input
format for the [LaTeX Note Importer][latex-note-importer].

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [amsthm-to-anki](#amsthm-to-anki)
    - [What Does it Do?](#what-does-it-do)
    - [Usage](#usage)
        - [Setting up Anki](#setting-up-anki)
        - [Setting up `amsthm` in your input document](#setting-up-amsthm-in-your-input-document)
    - [Code Outline](#code-outline)
        - [Libraries](#libraries)
        - [Internal API](#internal-api)
    - [TODO](#todo)

<!-- markdown-toc end -->


## What Does it Do?

 * Definitions, theorems, and lemmas with names are treated basically the same:
  - Definitions of the following (amsthm) form:
```tex
\begin{definition}[Name of Thing Being Defined]
  Definition of term.
\end{definition}
```
  - Theorems with names:
```tex
\begin{theorem}[Name of Theorem]
  An important theorem.
\end{theorem}
```
  - Lemmas with names:
```tex
\begin{lemma*}[Name of Lemma]
  An important lemma.
\end{lemma*}
```
  where "What is the term/theorem/lemma 'Name of Term/Theorem/Lemma'?" will be
  on the front and the definition/theorem/lemma will be on the back.
 * Theorems, lemmas, and corrolaries of an if-then form with premises and a
   conclusion:
```tex
\begin{theorem}[Name of Theorem]
  If
  \begin{premises}
    \item $x\in\C$
  \end{premises}
  then
  \begin{conclusion}
    there exists $y\in\C$ such that $y^2=x$.
  \end{conclusion}
\end{theorem}
```
  where the name/premises will be on the front side and the conclusion on the
  back.
 * Equations of a specific form:
```tex
\begin{eqenv}[Name of Equation]
  Setup ("Let x be a real number", etc.)
  \begin{equation*}
    x = y
  \end{equation*}
\begin{eqenv}
```
  where the setup and first half of the equation will be on the front side and the 
  second half will be on the back side. Note that the "halves" are split on the
  string `" = "`.
 * And things that are already notes will be kept:
```tex
\begin{note}
  \begin{field}
    Side 1
  \end{field}
  \begin{field}
    Side 2
  \end{field}
\end{note}
```

## Usage

You can download binaries from [the tags page][tags]. Then if `input.tex` is a
LaTeX2e file with the above sorts of environments in it, you can just run
```
./amsthm-to-anki input.tex output.tex
```
and import `output.tex` with the [LaTeX Note Importer][latex-note-importer].

### Setting up Anki

 0. Make sure `pdflatex` is on your `PATH` (hint: it likely already is).
 1. Install the [LaTeX Note Importer][latex-note-importer].
    See the Anki documentation for details on installing plugins.
 2. Ensure your LaTeX preamble is set how you like it: go to the "Add" screen.
    Click on the "Basic" button. Click "Manage", then select the first "Basic"
    card type from the list, and click "Options". Adjust your LaTeX preamble so
    that your notes will compile properly.

### Setting up `amsthm` in your input document

This is the setup I have in my document preamble:
```latex
\usepackage{amsthm}
\theoremstyle{plain}
\newtheorem{theorem}{Theorem}
\newtheorem*{theorem*}{Theorem}
\newtheorem{lemma}{Lemma}
\newtheorem*{lemma*}{Lemma}
\newtheorem{corrolary}{Corrolary}
\newtheorem*{corrolary*}{Corrolary}
\theoremstyle{definition} % these are the non-italicized
\newtheorem{eqenv}{Equation}
\newtheorem*{eqenv*}{Equation}
\newtheorem{remark}{Remark}
\newtheorem*{remark*}{Remark}
\newtheorem{example}{Example}
\newtheorem*{example*}{Example}
\newtheorem{definition}{Definition}
\newtheorem*{definition*}{Definition}
```
Note that the evironment names must match _exactly_ with what I've defined here
(although the displayed text -- the second argument -- can be arbitrary). I also
define the following environments:
```latex
\newenvironment{note}{\paragraph{Note:}}{}
\newenvironment{field}{}{}
\newenvironment{premises}{\begin{enumerate}[label=\alph*.]}{\end{enumerate}}
\newenvironment{conclusion}{}{}
```

## Code Outline
This section is mainly for my own benefit, and was used to outline the code
before I wrote it. Still, might be useful for those who come after!

As per the Stack default, `app/` contains the executable code, `src/` contains
the reusable code, and `test/` contains the tests for the code in `src/`, made
with the Tasty test framework, HUnit, QuickCheck, and Golden Test.

For each of the outlined transformations above, there is one file implementing
that extraction and one testing it:

 * `src/Extract.hs` 
 and `test/Extract.hs`
 * `src/Extract/DefinitionsTheoremsLemmas.hs` 
 and `test/DefinitionsTheoremsLemmas.hs`
 * `src/Extract/Premises.hs` 
 and `test/Premises.hs`
 * `src/Extract/Equations.hs` 
 and `test/Equations.hs`
 * `src/Extract/Notes.hs` 
 and `test/Notes.hs`
 
The `src/Extract/Util.hs` and `src/Types.hs` modules are imported by the
submodules of `Extract`. The `Main` module only imports the toplevel `Extract`
module, which provides a single entrypoint function. This maximizes the amount
of testable program logic.

### Libraries

I've used this project to experiment with
the [extensible-effects][extensible-effects] library, which provides an
alternative to monad transformers. Its full power wasn't necessary, I currently
only use it to provide a Writer monad in case I need debugging functionality.

Other newer or unusual libraries include:
 * Classy Prelude 
 * Tasty test framework: A composable way to integrate HUnit and QuickCheck.
 * `Data.Typeable` and `Data.Data`: I use these for testing. I wanted a concise
   way to write an HUnit assertion that the return value of a function uses a
   certain constructor, e.g. is a `Right`. Additionally, I didn't want to
   collapse the information to a `Bool` and only see `True /= False` when my
   tests failed. Check out `Test.Util` for more details. The ideas were adapted
   from [this StackOverflow answer][so]

### Internal API

Each module provides one method with the type
```haskell
moduleName ∷ LaTeX → ([Error], [Notecard])
```

## TODO
 
 * From: http://taylor.fausak.me/2016/12/05/haskell-package-checklist/
   - Use hpack, not Cabal
   - extra-source-files
   - doctest
 * Anki txt format
 * Better error messages

[amsthm]: http://www.ctan.org/pkg/amsthm
[anki]: http://ankisrs.net/
[latex-note-importer]: http://reh.math.uni-duesseldorf.de/~zibrowius/LatexNoteImporter/
[extensible-effects]: https://hackage.haskell.org/package/extensible-effects
[so]: http://stackoverflow.com/questions/25587501/test-if-a-value-matches-a-constructor
[tags]: https://github.com/siddharthist/amsthm-to-anki/tags
