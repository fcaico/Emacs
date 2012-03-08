/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import jde.debugger.Etc;
import com.sun.jdi.ObjectReference;
import jde.debugger.Rep;


/**
 * 'get_object' command. Information about a particular object.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_object objectID
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id {@link Rep#getObjectRep(ObjectReference, ObjectStore) detailed-object-info})
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class GetObject extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
 	
    if (args.size() < 1)
      throw new JDEException("Insufficient arguments");

    Long uniqueID = Etc.safeGetLong(args.remove(0), "object ID");
    ObjectReference oRef = proc.getStore().get(uniqueID);

    if (oRef == null) 
      throw new JDEException("No such object exists");

    jde.signalCommandResult(procID, cmdID, Rep.getObjectRep(oRef, proc.getStore(), true));
 }

  public Object clone() {return new GetObject();}
  
} // GetObject

/*
 * $Log: GetObject.java,v $
 * Revision 1.1  2001/03/24 05:52:13  paulk
 * Initial version.
 *
 *
 */

// End of Finish.java
