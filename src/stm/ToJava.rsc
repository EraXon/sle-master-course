module stm::ToJava

import stm::Syntax;
import ParseTree;
import List;
import IO;

/*

package <directory>;

public class <stm-name> {

    private static int <statename> = 0;

    private int current = 0;

    public void run(List<String> events) {
        for (String e: events) {
            switch (current) {

                case <stmname>: {
                    for each transitions 
                    if (event.equals(e)) {
                        current = <target>
                        break;
                    }
                    break;
                }
            }
        }
    }
}

*/

void compile(start[Machine] m) {
    loc j = m.src[extension="java"].top;
    writeFile(j, toJava(m));
}

str toJava(start[Machine] m) = toJava(m.top);

str toJava(m:(Machine)`machine <Id name> <State* ss>`)
  = "package stm;
    '
    'public class <m.src[extension=""].file> {
    '<for (lst := [ s | State s <- ss ], int i <- [0..size(lst)]) {>
    '   private static final int <lst[i].name> = <i>;
    '<}>    
    '   private int $current = 0;
    '
    '   public void run(java.util.List\<String\> events) {
    '     for (String $e: events) {
    '        switch ($current) {
    '          <for (State s <- ss) {>
    '            <state2case(s)>
    '          <}>
    '        }  
    '     }
    '   }
    '}";


str state2case((State)`state <Id name> <Trans* ts> end`)
  = "case <name>: {
    '  <for ((Trans)`<Id event> =\> <Id trg>` <- ts) {>
    '  if ($e.equals(\"<event>\")) {
    '      $current = <trg>;
    '  }
    '  <}>
    '  break;
    '}";