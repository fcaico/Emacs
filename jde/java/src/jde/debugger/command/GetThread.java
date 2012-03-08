/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import jde.debugger.Etc;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.ThreadReference;
import jde.debugger.Rep;


/**
 * 'get_thread' command. List a thread in more detail.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_thread threadID
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id 
 *  {@link Rep#getThreadRep(ThreadReference, ObjectStore, boolean)  detailed-thread-info})
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> The thread can be waiting for a monitor through entry
 * into a synchronized method, the synchronized
 * statement, or Object.wait(). The status() method can be used to
 * differentiate between the first two cases and the third. 
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class GetThread extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (args.size() < 1)
      throw new JDEException("Insufficient arguments");

    Long uniqueID = Etc.safeGetLong(args.remove(0), "thread ID");
    ObjectReference tRef = proc.getStore().get(uniqueID);
	    
    if (tRef == null) {
      throw new JDEException("No such thread exists");
    } else if (!(tRef instanceof ThreadReference)) {
      throw new JDEException("No such thread exists (anymore?)");
    }
	    
    jde.signalCommandResult(procID, cmdID, 
			    Rep.getThreadRep((ThreadReference)tRef, 
					     proc.getStore(), true));
  }



  public Object clone() {return new GetThread();}
  
} // GetThread

/*
 * $Log: GetThread.java,v $
 * Revision 1.1  2001/03/24 05:52:14  paulk
 * Initial version.
 *
 *
 */

// End of GetThread.java
