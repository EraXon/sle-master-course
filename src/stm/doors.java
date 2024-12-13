package stm;

public class doors {

   private static final int current = 0;

   private static final int opened = 1;

   private static final int locked = 2;
    
   private int $current = 0;

   public void run(java.util.List<String> events) {
     for (String $e: events) {
        switch ($current) {
          
            case current: {
              
              if ($e.equals("open")) {
                  $current = opened;
                  break;
              }
              
              if ($e.equals("lock")) {
                  $current = locked;
                  break;
              }
              
              break;
            }
          
            case opened: {
              
              if ($e.equals("close")) {
                  $current = current;
                  break;
              }
              
              break;
            }
          
            case locked: {
              
              if ($e.equals("e")) {
                  $current = current;
                  break;
              }
              
              break;
            }
          
        }  
     }
   }
}