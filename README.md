
# SLE Master Course

This document contains the information relevant for the master course [Software Language Engineering](https://ocasys.rug.nl/current/catalog/course/WMCS037-05) at University of Groningen. 

Note: the official course description refers to the book _Software Languages_, by Ralf Laemmel; I have decided to *not* use that book, but instead rely on supplemental material as indicated per week in the schedule below. Hence, the exam will be open "book", you may consult print-outs of the material during the exam. 

## Preliminaries for the lab

Please install [VS Code](https://code.visualstudio.com/) and then the [Rascal VS Code extension](https://marketplace.visualstudio.com/items?itemName=usethesource.rascalmpl) (you can also find it in the extension browser).

Fork [this](https://github.com/cwi-swat/sle-master-course) repository to your own Github account, and clone it to your local machine (_NB: don't rename the folder!_). Finally, go to the File menu of VS Code and select "Add Folder to workspace", navigate to where you've cloned the repo, and select that directory. Note: you have to fork the repo, otherwise you won't be able to commit/push!

The project comes with pre-wired IDE support for the Questionnaire Language (QL). 
To enable this: open the file `IDE.rsc`, and press the link at the top of the file "Import in new Rascal Terminal". Then issue the following command in the terminal: `main();` (followed by enter).
As soon your implementation of various components progresses the IDE support will start working. For instance, if you finish your grammar, and reissue `main()` (see above) then syntax highlighting will be enabled. 

## QL: a DSL for Questionnaires

The course is based on a domain-specific language (DSL) for questionnaires, called QL. A QL program consists of a form, containing questions. A question can be a normal question, that expects an answer (i.e. is answerable), or a computed question. A computed question has an associated expression which defines its value. 

Both kinds of questions have a prompt (to show to the user), an identifier (its name), and a type. The language supports boolean, integer and string types.

Questions can be conditional and the conditional construct comes in two variants: **if** and **if-else**. A block construct using `{}` can be used to group questions.

Questions are enabled and disabled when different values are entered, depending on their conditional context.

Here’s a simple questionnaire in QL from the domain of tax filing:
```
form "Tax office example" { 
  "Did you sell a house in 2010?" // the prompt of the question
    hasSoldHouse: boolean         // and its name and type
  "Did you buy a house in 2010?"
    hasBoughtHouse: boolean
  "Did you enter a loan?"
    hasMaintLoan: boolean
    
  if (hasSoldHouse) { // conditional block
    "What was the selling price?"
      sellingPrice: integer
    "Private debts for the sold house:"
      privateDebt: integer
    "Value residue:"
      valueResidue: integer =      // a computed question
        sellingPrice - privateDebt // has an expression 
  }
}
```

A full type checker of QL detects:
- references to undefined questions
- duplicate question declarations with different types
- conditions that are not of the type boolean
- operands of invalid type to operators
- duplicate prompts for different questions (warning)
- cyclic data and control dependencies
- warnings for empty conditional bodies (both if and else)
- warning for empty prompts
- warning for useless conditions (i.e., true/false)

Different data types in QL map to different (default) GUI widgets. For instance, boolean would be represented as a checkbox, integers as text fields with numeric sliders, and strings as text fields. The HTML form corresponding to the QL program from above could look as follows in Chrome:

<img src="https://github.com/cwi-swat/testing-dsls-with-dsls/blob/main/examples/tax.png" alt="Screenshot of the tax form" width="280" height="400" />

See the folder `examples/` for example QL programs. Opening a QL file will show links at the top for compiling, and running QL programs. 
Running a questionnaire immediately opens a browser pane in VS Code. Compiling will result in an HTML file and Javascript file, as soon as you finish the compiler. Opening
them in a browser will again execute the questionnaire (possibly with a slightly different layout).


## TestQL: a DSL for testing the QL DSL

TestQL is DSL for testing QL. In fact, it's an extension of the QL language using Rascal's support for extensible syntax definition. Using TestQL, tests can be expressed in a human readable, declarative format. 

TestQL files end with the extension `testql`, and have IDE support enabled, just like the QL programs written in files with the `myql` extension. For the test files the IDE support includes clickable links:

- to execute the whole test suite 
- to show test coverage of the test suite (i.e. how much of the syntax of the DSL has been covered by tests.)
- to run an individual test.

You can find different types of tests in the file `alltests.ql`, divided over different sections: static checking, dynamics semantics and syntax, and rendering.
You should use this file to test your implementation of the type checker and the interpreter. 

## Course Schedule

For dates, times and rooms: https://rooster.rug.nl/#/en/current/schedule/course-WMCS037-05/timeRange=all 

### Week 1

Lecture: Domain-specific languages. Introduction to the course. 

Lab: warm-up exercises (see `tutorial/Series1.rsc` and `tutorial/Series2.rsc`).

Background material: When and how to develop domain-specific languages, https://doi.org/10.1145/1118890.1118892

### Week 2

Lecture: syntax and structure. Parsing, disambiguation, Abstract Syntax Trees (ASTs), name binding.


Lab: syntax definition of QL. Name resolution. `Syntax.rsc`, `Resolve.rsc`.

Background material: Pure and declarative syntax definition: paradise lost and regained, https://doi.org/10.1145/1869459.1869535 

### Week 3

Lecture: static checking. 

Lab: type checker for QL, cyclic dependency check. `Check.rsc` 

Background material: Type Systems, https://doi.org/10.1145/234313.234418 

### Week 4

Lecture: interpreters, Salix library.

Lab: evaluator, renderer (abstract) in `Eval.rsc`; web app in `App.rsc`. Think about division by zero (NB: QL does not feature exceptions of any sort).

### Week 5

Lecture: code generation

Lab: compiler for QL to HTML and plain Javascript (do not use a framework!). Ensure you implement the fixpoint behavior (see `Eval.rsc`) correctly, and be aware that Javascript has floats but not integers. `Compile.rsc`

Background material: Code Generation by Model Transformation,  https://doi.org/10.1007/978-3-540-69927-9_13

### Week 6

Lecture: program transformation and model transformation. Partial evaluation. 

Lab: flattening, formatting (pretty printing), dependency visualization, partial evaluation. `Flatten.rsc`, `Format.rsc`, `Visualize.rsc`, `PartialEvaluation.rsc`.

Background material: 
- Program transformation mechanics, https://dspace.library.uu.nl/handle/1874/24002 
- Tutorial Online Partial Evaluation, https://doi.org/10.48550/arXiv.1109.0781

### Week 7

Lecture: modularity, evolution, language extension

Lab: pick one of the following language extensions and modify/evolve/extend *all* relevant aspects of your base QL implementation to support it. Think about modularity!

- Enum data type. Allow declaration of an enum as part of a form, with alternatives (and their human-readable labels); ensure type checking when used in expressions (which kinds are relevant?). Render as a (multi-select) dropdown list in HTML. Possible syntax:
    ```
    enum Country {
        "Belgium" be
        "The Netherlands" nl
        "Other" other
    }
    "Where do you live?" country: Country
    if (country == other) {
        ...
    }
    ```
Think about how the type checker should deal with the constants vs. question names. 

- Data validation plus errors on answerable questions: non-emptiness, ranges for integers (which may refer to integer-typed questions), regular expression for string. At run-time these constraints should be checked, and the user should be notified. Possible syntax:
    ```
    "What is your age?" age: integer [1..99] // an integer range
    "What is your name?" name: string [required] // i.e. cannot be empty
    "What is your zipcode?" zip: string [/[0-9][0-9][0-9][0-9] [A-Z][A-Z]/]
    ```

- Iteration and lists. Sometimes you want to repeat a certain sub questionnaire a number of times. Make sure that the lists are represented properly in the Questionnaire state. Possible syntax:
    ```
    "How many kids do you have?" numOfKids: integer
    repeat (numOfKids) {
        "First name?" firstName: string
        "Age?" age: integer
    }

- Pages and sections. Sometimes it would be nice to structure a large questionnaire into pages and sections. A page contains questions that are on the screen at the same time, with navigation buttons (e.g., next, previous). A section groups sets of questions. Both pages and sections have titles. Possible syntax:
    ```
    page "Start" {
        section "Personalia" {
            ...
        }
        section "Finance" {
            ...
        }
    }
    page "Next page" {
        ....
    }
    ```
Think about possible static errors or warnings. 

- Record types. Records group related data items, and can be referred to in one go. The record type includes "prompts" for its elements, so that can be rendered as questions. Possible syntax:
    ```
    record Address {
        "Street?" street: string
        "Number?" number: string
        ...
    }
    "Address" address: Address // renders as form group titled Address
    ```
Extend the expression language with field access for record types. Type check that fields are correctly referenced. Make sure the record values are representable in the questionnaire state. 

- Currency data type: extend QL with a `currency[x]` data type where `x` represents the name of a currency (e.g. euro, dollar etc.). Type check that different currencies are not added/subtracted. Check that only addition and subtraction are allowed, plus division/multiplication with an integer or a percentage (add this operator). At run-time ensure that computations are executed correctly w.r.t. rounding. Don't use JS's floats for this.  Render currencies *as* currencies. NB: this introduces operator overloading in QL. 

- Modular QL: allow QL files to be divided into modules, and introduce an import statement to "include" (think about what that means!) one module into another. Extend the type checker so that modules are type checked against the signature  (e.g., the set of exported question names) of its imports. Make sure that name resolution deals with multiple modules. Introduce a normalization phases so that you can reuse the compiler. Possible syntax:
    ```
    module Tax {
      ... // some questions
      if (hasSoldHouse) {
        import Housing
      }
    }

    form "Tax filing" {
      // a top-level form
      "What is your name?" name: string
      import Tax
      import Finance
    }
    ```

Background material: 
- Language Composition Untangled, https://doi.org/10.1145/2427048.2427055
- Modular language implementation in Rascal – experience report, https://doi.org/10.1016/j.scico.2015.11.003



### Week 8

Wrap-up, checking lab solutions.
