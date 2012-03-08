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
import jde.debugger.ObjectStore;


/**
 * 'get_object_monitors' command. Information about the monitors
 * corresponding to a particular object.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_object_monitors objectID
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id 
 *       {@link Rep#getObjectMonitorsRep(ObjectReference, ObjectStore) object-monitors-info})
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class GetObjectMonitors extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (args.size() != 1)
      throw new JDEException("Insufficient arguments");
	
    Long uniqueID = Etc.safeGetLong(args.remove(0), "object ID");
    ObjectStore store = proc.getStore();
    ObjectReference oRef = store.get(uniqueID);
    if (oRef == null) 
      throw new JDEException("No such object exists");

    jde.signalCommandResult(procID, cmdID, Rep.getObjectMonitorsRep(oRef,store));
  }



  public Object clone() {return new GetObjectMonitors();}
  
} // GetObjectMonitors

/*
 * $Log: GetObjectMonitors.java,v $
 * Revision 1.1  2001/03/24 05:52:14  paulk
 * Initial version.
 *
 *
 */

// End of GetObjectMonitors.java
