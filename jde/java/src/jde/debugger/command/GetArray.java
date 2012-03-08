/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.2 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import jde.debugger.Etc;
import com.sun.jdi.ObjectReference;
import jde.debugger.Rep;
import com.sun.jdi.ArrayReference;

/**
 * 'get_array' command. Information about a given array, and,
 * optionally, values of a range of indices
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_array objectID [index, length]
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id {@link Rep#getArrayRep(ArrayReference, ObjectStore, int, int) array})
 * </pre>
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 */
public class GetArray extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {

    if (args.size() < 1)
      throw new JDEException("Insufficient arguments");

    Long uniqueID = Etc.safeGetLong(args.remove(0), "object ID");
    ObjectReference oRef = proc.getStore().get(uniqueID);
	    
    if (oRef == null) {
      throw new JDEException("No such object exists");
    } else if (!(oRef instanceof ArrayReference)) {
      throw new JDEException("Object is not an array");
    }

    if (args.size() == 0) {
      jde.signalCommandResult(procID, cmdID, Rep.getArrayRep((ArrayReference)oRef, proc.getStore(), -1, -1));
    } else if (args.size() == 2) {
      int index = Etc.safeGetint(args.remove(0), "index");
      int length = Etc.safeGetint(args.remove(0), "length");
      jde.signalCommandResult(procID, cmdID, Rep.getArrayRep((ArrayReference)oRef, 
						      proc.getStore(), index, length));
    } else {
      throw new JDEException("Syntax error: Wrong number of arguments");
    }
 
  }

  public Object clone() {return new GetArray();}
  
} // GetArray

/*
 * $Log: GetArray.java,v $
 * Revision 1.2  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.1  2000/03/03 07:08:39  paulk
 * Initial revision.
 *
 */

// End of GetArray.java
