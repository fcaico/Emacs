/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.2 $
 */

package jde.debugger.command;
import java.util.List;
import jde.debugger.JDEException;
import jde.debugger.Jdebug;
import jde.debugger.DebuggeeProcess;
import com.sun.jdi.request.EventRequest;
import java.util.Map;
import com.sun.jdi.request.EventRequestManager;
import java.util.HashMap;
import java.util.Collections;


/**
 * DebugProcessCommand.java
 *
 *
 * Created: Fri Jan 28 21:58:06 2000
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 */

abstract public class DebugProcessCommand extends DebugCommand {
  
  public void init(Integer procID, Integer cmdID, 
		   String cmdName, List args) throws JDEException {
    super.init(procID, cmdID, cmdName, args);

    // this app id should be valid.  
    if (!procRegistry.processExists(procID)) 
	throw new JDEException("Application "+ procID + " does not exist");

    proc = procRegistry.getProcess(procID);

  }


    /**
     * Adds an event request to the above map. Also enables the request.
     *
     * @return an identifier for the request
     */
    protected Long addIdentifiableRequest(EventRequest e) {
	Long id;
	synchronized (identifiableEventRequests) {
	    id = proc.generateObjectID();
	    identifiableEventRequests.put(id, e);
	}
	e.enable();
	return id;
    }

    /**
     * Removes an event request. Also disables/deletes from the vm.
     */
    protected void deleteIdentifiableRequest(Long id)
	throws JDEException {

	EventRequestManager erm = proc.getVM().eventRequestManager();

	synchronized (identifiableEventRequests) {
	    if (!identifiableEventRequests.containsKey(id)) {
		throw new JDEException("Invalid request ID");
	    } else {
		Object e = identifiableEventRequests.get(id);
		if (e == null) {
		    throw new JDEException("No such event request");
		} else if (e instanceof EventRequest) {
		    ((EventRequest)e).disable();
		    erm.deleteEventRequest((EventRequest)e);
		} else {
		    throw new JDEException("Not an event request");
		}
	    }
	}
    }
	

  protected DebuggeeProcess proc;

    /**
     * This map stores the event requests that are NOT specs. storing
     * it here allows the user to cancel them easily: they just specify the
     * id, that gets reverse-looked up here, uniquely identifying the actual
     * request.
     * <p>
     * Of course, the id is sent back to the user when the actual command is
     * responded to, so that the handle is available to jde in the first
     * place
     */ 
    protected Map identifiableEventRequests =
	Collections.synchronizedMap(new HashMap());

  
} // DebugApplicationCommand


/*
 * $Log: DebugProcessCommand.java,v $
 * Revision 1.2  2001/07/06 02:05:51  paulk
 * Makefile
 *
 * Revision 1.1  2001/03/24 05:48:40  paulk
 * Initial version.
 *
 * Revision 1.2  2000/03/03 07:40:32  paulk
 * Converted get_string and get_array commands from functions to objects.
 *
 * Revision 1.1  2000/01/31 12:46:10  paulk
 * Defines general behavior of application debug commands.
 *
 */

// End of DebugProcessCommand.java
