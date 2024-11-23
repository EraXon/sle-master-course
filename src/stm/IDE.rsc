module stm::IDE

import util::LanguageServer;
import util::Reflective;
import util::IDEServices;
import IO;
import ValueIO;
import List;

import stm::Syntax;
import Message;
import ParseTree;


set[LanguageService] stmLanguageContributor() = {
    parser(Tree (str input, loc src) {
        return parse(#start[Machine], input, src);
    })
    // ,
    // lenses(myLenses),
    // executor(myCommands),
    // summarizer(mySummarizer
    //     , providesDocumentation = false
    //     , providesDefinitions = true
    //     , providesReferences = false
    //     , providesImplementations = false)
};


void main() {
    registerLanguage(
        language(
            pathConfig(srcs = [|std:///|, |project://sle-master-course/src|]),
            "State machines",
            "stm",
            "stm::IDE",
            "stmLanguageContributor"
        )
    );
}


