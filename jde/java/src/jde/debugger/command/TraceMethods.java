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
import java.util.List;
import com.sun.jdi.request.EventRequestManager;
import com.sun.jdi.request.MethodEntryRequest;
import java.util.Iterator;
import com.sun.jdi.request.MethodExitRequest;


    /**
     * 'trace_methods' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * trace_methods <u>type</u>
     *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
     *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
     *      [{@link Etc#getClassFiltersFromArgs(List) class-filters}]
     *      [{@link Etc#getClassExFiltersFromArgs(List) class-exclusion-filters}]
     * </pre>
     *
     * <b>Returns:</b>
     * <pre>
     * (jde-dbo-command-result cmd_id <u>requestID</u>)
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> <u>type</u> is either "entry" or "exit"
     * <li> Use <u>requestID</u> to cancel the trace request.
     * </ul>
     *
     * <p>
     * @see EventHandler#methodEntryEvent(MethodEntryEvent)
     * @see EventHandler#methodExitEvent(MethodExitEvent)
     *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class TraceMethods extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (args.size() < 2)
      throw new JDEException("Insufficient arguments");

    String type = args.remove(0).toString().toLowerCase();
    if (!(type.equals("entry") || type.equals("exit")))
      throw new JDEException("Invalid type");

    Object thread = Etc.getThreadFromArgs(args);
    ObjectReference tRef = null;
    if (thread == null) {
      tRef = null;
    } else if (thread instanceof Long) {
      tRef = (ObjectReference)proc.getStore().get(thread);
      if (tRef == null) {
	throw new JDEException("No such thread exists");
      } else if (!(tRef instanceof ThreadReference)) {
	throw new JDEException("No such thread exists (anymore?)");
      }
    } else if (thread instanceof String) {
      tRef = proc.getThread(thread.toString());
    }

    List classFilters = Etc.getClassFiltersFromArgs(args);
    List classExFilters = Etc.getClassExFiltersFromArgs(args);

    Long requestID = null;
	
    EventRequestManager em = proc.getVM().eventRequestManager();

    if (type.equals("entry")) {

      MethodEntryRequest mer = em.createMethodEntryRequest();

      if (tRef != null) 
	mer.addThreadFilter((ThreadReference)tRef);

      mer.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));

      if (classFilters != null) {
	Iterator it = classFilters.iterator();
	while (it.hasNext())
	  mer.addClassFilter(it.next().toString());
      }
      if (classExFilters != null) {
	Iterator it = classExFilters.iterator();
	while (it.hasNext())
	  mer.addClassExclusionFilter(it.next().toString());
      }
      requestID = addIdentifiableRequest(mer);
	    
    } else if (type.equals("exit")) {

      MethodExitRequest mer = em.createMethodExitRequest();

      if (tRef != null) 
	mer.addThreadFilter((ThreadReference)tRef);
	    
      mer.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));
	    
      if (classFilters != null) {
	Iterator it = classFilters.iterator();
	while (it.hasNext())
	  mer.addClassFilter(it.next().toString());
      }
      if (classExFilters != null) {
	Iterator it = classExFilters.iterator();
	while (it.hasNext())
	  mer.addClassExclusionFilter(it.next().toString());
      }
      requestID = addIdentifiableRequest(mer);
    }
    jde.signalCommandResult(procID, cmdID, requestID);
  }

  public Object clone() {return new TraceMethods();}
  
} // TraceMethods

/*
 * $Log: TraceMethods.java,v $
 * Revision 1.1  2001/03/24 13:35:26  paulk
 * Initial revision.
 *
 *
 */

// End of TraceMethods.java
