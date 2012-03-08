package jde.debugger;
import java.io.StreamTokenizer;
import java.io.BufferedReader;
import java.util.List;
import java.util.ArrayList;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.InputStreamReader;


/**
 * CommandStream.java
 *
 *
 * Created: Tue Feb 13 15:40:34 2001
 *
 * @author <a href="mailto: "</a>
 * @version
 */

public class CommandStream extends StreamTokenizer {

  public CommandStream (BufferedReader in){
    
    super(in);
    setSyntax();
  }

  public List nextCommand() {
    
    List commandLine = new ArrayList();
    
    try {

      int token = nextToken();
      
      while (token != TT_EOL) {

	switch (token) {

	case TT_EOF :     
	  throw new IOException("EOF occurred reading command stream.");
	  
	case TT_WORD :
	case '"' : 
	case '\'': 
	  commandLine.add(sval);
	  break;
	    	  
	default:
	   commandLine.add(String.valueOf((char)token));
	  break;
	} // end of switch ()      
	
	token = nextToken();
      }

      if (commandLine.size() < 3) {
	if (commandLine.size() > 0) 
	  JDE.getJDE().signalCommandError(Jdebug.debuggerID, new Integer(-1), "Malformed command");
	commandLine = nextCommand();
      }
    }
    catch (IOException ex) {
      commandLine = null;
    } // end of catch 

    return commandLine;
  }

  /**
   * Sets the syntax of the command stream. We want the input to be broken
   * into lines, with whitespaces separating tokens
   */
  private void setSyntax() {
    resetSyntax();
    eolIsSignificant(true);
    whitespaceChars('\u0000', '\u0020');
    wordChars('\u0021', '\u00ff');
    quoteChar('"');
  }	

  public static void main (String[] args) {

    PrintWriter out = new PrintWriter(System.out);
    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
    CommandStream commandStream = new CommandStream(in);

    out.print("> ");
    out.flush();
    List command = commandStream.nextCommand();
    while (command != null && ! ((String) command.get(2)).startsWith("qu")) {

      int n = command.size();
      for (int i = 0; i<n; i++) {
	out.println(command.get(i));	
      } // end of for ()
      
      out.print("> ");
      out.flush();
      command = commandStream.nextCommand();      
    } // end of while ()
    

    

  } // end of main ()
  
 
		
}// CommandStream
