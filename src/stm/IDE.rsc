module stm::IDE

import util::LanguageServer;
import util::Reflective;
import util::IDEServices;
import IO;
import ValueIO;
import List;

import stm::Syntax;
import stm::Check;
import Message;
import ParseTree;


set[LanguageService] stmLanguageContributor() = {
    parser(Tree (str input, loc src) {
        return parse(#start[Machine], input, src);
    }),
    summarizer(mySummarizer
        , providesDocumentation = false
        , providesDefinitions = true
        , providesReferences = false
        , providesImplementations = false)
};

Summary mySummarizer(loc origin, start[Machine] m) {
  RefGraph g = resolve(m);
  set[Message] msgs = check(m, g);
  rel[loc, Message] msgMap = {< m.at, m> | Message m <- msgs };
  
  return summary(origin, messages = msgMap, definitions = g.useDef);
}

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


