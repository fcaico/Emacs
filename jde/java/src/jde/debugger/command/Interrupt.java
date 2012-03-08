/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import java.util.Iterator;
import jde.debugger.Etc;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.ThreadGroupReference;


/**
 * 'interrupt' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * interrupt [threadID]+
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> threadID can be retrieved using the get_threads command
 * <li> at least one threadId should be specified
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class Interrupt extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (args.size() < 1) 
      throw new JDEException("Insufficient arguments");

    Iterator it = args.iterator();
    while (it.hasNext()) {
      Long uniqueID = Etc.safeGetLong(it.next(), "thread ID");

      ObjectReference oRef = (ObjectReference)proc.getStore().get(uniqueID);
      if (oRef == null) {
	throw new JDEException("Invalid ThreadID, or the thread is dead");
      } else if (oRef instanceof ThreadReference) {
	((ThreadReference)oRef).interrupt();
      } else {
	throw new JDEException("The object is not a thread");
      }
    }
    jde.signalCommandResult(procID, cmdID);
  }

  public Object clone() {return new Interrupt();}
  
} // Interrupt

/*
 * $Log: Interrupt.java,v $
 * Revision 1.1  2001/03/24 13:35:25  paulk
 * Initial revision.
 *
 *
 */

// End of Interrupt.java
