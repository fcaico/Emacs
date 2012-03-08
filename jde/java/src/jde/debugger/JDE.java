package jde.debugger;
import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Iterator;

/**
 * JDE.java
 *
 *
 * Created: Thu Feb 15 12:58:59 2001
 *
 * @author <a href="mailto: "</a>
 * @version
 */

public class JDE implements Protocol {


  private JDE (){}


  public void init() {

    // The debugger uses standard out to sent command responses, error
    // messages and event notifications to the JDE.
    out = new PrintWriter(System.out);

    // The debugger uses standard int to read commands from the JDE.
    in = new BufferedReader(new InputStreamReader(System.in));

    commandStream = new CommandStream(in);
    
  }


  public List nextCommand() {
    return commandStream.nextCommand();
  }

  /*
   *
   * FUNCTIONS FOR SENDING INFORMATION OVER TO THE JDE SIDE
   *
   */


  /**
   * Returns a string representation of the object. Here is the logic:
   * <ul>
   * <li> If the object is null, return an empty string
   * <li> If the object is string, quote it within quotation marks.
   * <li> If the object is a list, recursively call stringRep, appending
   *      a space.
   * <li> If it's any other kind of object, return the .toString()
   * </ul>
   */
  public String stringRep(Object obj) {
    if (obj == null) {
      return "";
    } else if (obj instanceof String) {
      return "\""+obj.toString()+"\"";
    } else if (obj instanceof List) {
      StringBuffer returnString = new StringBuffer("");
      Iterator it = ((List)obj).iterator();
      while (it.hasNext()) {
	returnString.append(stringRep(it.next())+" ");
      }
      return  BR +returnString.toString().trim();
    } else {
      return obj.toString();
    }
  }


  /**
   * Send an arbitrary lisp function across.
   *
   * @param app_id The application ID
   * @param type The function name. JDE_BUG gets added to
   * its beginning
   * @param obj An arbitrary object. If a string, it's just printed out,
   * if a list, each of its elements is printed out, with a space after
   * each.
   */
  synchronized public void signal(Integer app_id, String type,
				  Object obj) {
    String strRep = stringRep(obj);
    if (strRep.equals("")) {
      System.out.println(BR+"("+JDE_BUG+type+" "+app_id+")"+BR);
    } else {
      String temp="("+JDE_BUG+type+ BR +app_id+" "+stringRep(obj)+")";
      if (temp.length() <= 80)
	System.out.println(BR+"("+JDE_BUG+type+" "+app_id+" "+
		    stringRep(obj)+")"+BR);
      else
	System.out.println( BR +temp+ BR );
    }
    System.out.flush();
  }

  /**
   * Signal a reply: a result or an error
   */
  synchronized private void signalReply(Integer app_id, Integer cmd_id,
					Object obj, String type) {
    String strRep = stringRep(obj);
    if (strRep.equals("")) {
      System.out.println(BR+"("+type+" "+cmd_id+")"+BR);
    } else {
      String temp = "("+type+ BR +cmd_id+" "+strRep+")";
      if (temp.length() <= 80)
	System.out.println(BR+"("+type+" "+cmd_id+" "+strRep+")"+BR);
      else 
	System.out.println( BR +temp+ BR );
    }	    
    System.out.flush();
  }
    

  /**
   * send the result of a command. indicates a positive completion of
   * the command. this could of course be provisional: eg. in case of
   * provisional breakpoints
   */
  public void signalCommandResult(Integer app_id, Integer cmd_id) {
    signalCommandResult(app_id, cmd_id, null);
  }

  public void signalDebug(String msg) {
    signal(Jdebug.debuggerID, MESSAGE, msg);
  } 

  /**
   *
   * the result of a command. if it's a string, just send it across, else
   * it should be a list. each element is sent across, just doing a
   * toString() -> ie, if you want to send (... 23 "some string" 45), you
   * need to put in the quotes (ie "") yourself: else what will be sent
   * will be (... 23 some string 45), obviously wrong.
   */
  synchronized public void signalCommandResult(Integer app_id,
					       Integer cmd_id, Object obj) {
    signalReply(app_id, cmd_id, obj, COMMAND_RESULT);
  }

    
  /**
   * reply to a command with an error. 
   * @param obj Is usually a string explaining what went wrong.
   */
  synchronized public void signalCommandError(Integer app_id,
					      Integer cmd_id, Object obj) {
    signalReply(app_id, cmd_id, obj, COMMAND_ERROR);
  }

  public static JDE getJDE() {
    return jde;
  }

  /*
   * Reads commands from JDE.
   */
  BufferedReader in;

  /*
   * Writes command responses, messages, and event notifications to 
   * JDE.
   */
  PrintWriter out;


  CommandStream commandStream;

  private static JDE jde = new JDE();


}// JDE
