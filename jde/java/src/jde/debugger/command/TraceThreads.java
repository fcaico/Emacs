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
import java.util.List;
import com.sun.jdi.request.EventRequestManager;
import com.sun.jdi.request.ThreadStartRequest;
import com.sun.jdi.request.ThreadDeathRequest;


/**
 * 'trace_threads' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * trace_threads <u>type</u> [threadID]
 *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> <u>type</u> can be either "start" or "death"
 * <li> If no threadID is specified, all the corresponding thread
 * events are raised.
 * </ul>
 *
 * <p>
 * @see EventHandler#threadStartEvent(ThreadStartEvent)
 * @see EventHandler#threadDeathEvent(ThreadDeathEvent)
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class TraceThreads extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (args.size() < 2)
      throw new JDEException("Insufficient arguments");

    String type = args.remove(0).toString().toLowerCase();

    if (!(type.equals("start") || type.equals("death")))
      throw new JDEException("Invalid type");

    List classFilters = Etc.getClassFiltersFromArgs(args);
    List classExFilters = Etc.getClassExFiltersFromArgs(args);

    EventRequestManager em = proc.getVM().eventRequestManager();
	
    Long requestID = null;
    ObjectStore store = proc.getStore();

    if (type.equals("start")) {

      ThreadStartRequest ter = em.createThreadStartRequest();

      ter.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));

      if (args.size() > 0) {
	Long threadID = Etc.safeGetLong(args.remove(0), "thread ID");
	ObjectReference tRef = store.get(threadID);
	if (tRef == null) {
	  throw new JDEException("No such thread exists");
	} else if (!(tRef instanceof ThreadReference)) {
	  throw new JDEException("No such thread exists (anymore?)");
	}
	ter.addThreadFilter((ThreadReference)tRef);
      }

      requestID = addIdentifiableRequest(ter);
	    
    } else if (type.equals("death")) {

      ThreadDeathRequest ter = em.createThreadDeathRequest();

      ter.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));
	    
      if (args.size() > 0) {
	Long threadID = Etc.safeGetLong(args.remove(0), "thread ID");
	ObjectReference tRef = store.get(threadID);
	if (tRef == null) {
	  throw new JDEException("No such thread exists");
	} else if (!(tRef instanceof ThreadReference)) {
	  throw new JDEException("No such thread exists (anymore?)");
	}
	ter.addThreadFilter((ThreadReference)tRef);
      }
	    
      requestID = addIdentifiableRequest(ter);
    }
    jde.signalCommandResult(procID, cmdID, requestID);
  }



  public Object clone() {return new TraceThreads();}
  
} // TraceThreads

/*
 * $Log: TraceThreads.java,v $
 * Revision 1.1  2001/03/24 13:35:26  paulk
 * Initial revision.
 *
 *
 */

// End of TraceThreads.java
