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
import com.sun.jdi.InvalidTypeException;


/**
 * 'kill thread' command. Kill a thread with a given exception object.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * kill_thread threadID exceptionObjectID
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> threadID can be retrieved using the get_threads command
 * <li> exceptionObjectID is the object id of a Throwable object. It
 * can be created using the 'evaluate' command, or an existing throwable
 * object can be used.
 * </ul>
 * 
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class KillThread extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (args.size() < 2) 
      throw new JDEException("Insufficient arguments");
	
    Long uniqueID = Etc.safeGetLong(args.remove(0), "thread ID");

    ObjectReference oRef = proc.getStore().get(uniqueID);
    if (oRef == null) {
      throw new JDEException("No such thread exists");
    } else if (!(oRef instanceof ThreadReference)) {
      throw new JDEException("The ID doesn't correspond to a thread");
    }
    ThreadReference tRef = (ThreadReference)oRef;

    uniqueID = Etc.safeGetLong(args.remove(0), "thread ID");

    oRef = proc.getStore().get(uniqueID);
    if (oRef == null) {
      throw new JDEException("No such thread exists");
    }

    try {
      tRef.stop(oRef);
    } catch (InvalidTypeException ex) {
      throw new JDEException("Object ID doesn't correspond to a Throwable object");
    }
    jde.signalCommandResult(procID, cmdID);
  }

  public Object clone() {return new KillThread();}
  
} // KillThread

/*
 * $Log: KillThread.java,v $
 * Revision 1.1  2001/03/24 13:35:25  paulk
 * Initial revision.
 *
 *
 */

// End of KillThread.java
