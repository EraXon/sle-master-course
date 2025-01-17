module CheckPage


import Check;
import Message;
import IO;
import ParseTree;
import List;
import vis::Text;

void checkPages(start[Form] f){
    f=f.top;
    //println("Checking pages");
    if (f has pages){
        //println("Form has pages");
        for (page <- f.pages){
            Str title=page.title;
            Form f=(Form)`form <Str title> {}`;
            list[Question] questions = [];
            for (section <- page.sections){

                for (question <- section.questions){
                    questions += question;
                    //println(question);
                }
            }

            for (Question q <- questions, (Form)`form <Str t> {<Question* qqs>}` := f) {
                f = (Form)`form <Str t> {
                  '  <Question* qqs>
                  '  <Question q>
                  '}`;
    }   

            println(f);
            println("Checking page <page.title>");


        }

    }



}