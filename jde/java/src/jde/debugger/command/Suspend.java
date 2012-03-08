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
 * 'suspend' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * suspend [threadID]*
 *
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> threadIDs can be retrieved using the get_threads command
 * <li> if the list is omitted, the entire VM is suspended
 * <li> threadIDs can refer to either threads or threadgroups.
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class Suspend extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    // see if there are arguments (should be the thread id). if so, we
    // suspend the thread ids passed. else, suspend the whole vm.
    if (args.size() > 0) {
      Iterator it = args.iterator();
      while (it.hasNext()) {
		
	Long uniqueID = Etc.safeGetLong(it.next(), "thread(group)");
		    
	ObjectReference oRef = (ObjectReference)proc.getStore().get(uniqueID);
	if (oRef == null) {
	  throw new JDEException("Invalid ThreadID, or the thread/threadgroup is dead");
	} else if (oRef instanceof ThreadReference) {
	  ((ThreadReference)oRef).suspend();
	} else if (oRef instanceof ThreadGroupReference) {
	  ((ThreadGroupReference)oRef).suspend();
	} else {
	  throw new JDEException("The object is not a thread or a threadgroup");
	}
      }
      jde.signalCommandResult(procID, cmdID);
    } else {
      try {
	proc.getVM().suspend();
	jde.signalCommandResult(procID, cmdID);
      } catch (Exception ex) {
	throw new JDEException("Unable to suspend the application");
      }
    }
  }

  public Object clone() {return new Suspend();}
  
} // Suspend

/*
 * $Log: Suspend.java,v $
 * Revision 1.1  2001/03/24 13:35:25  paulk
 * Initial revision.
 *
 *
 */

// End of Suspend.java
