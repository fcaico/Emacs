/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.3 $
 */

package jde.debugger.command;
import com.sun.jdi.connect.AttachingConnector;
import jde.debugger.JDEException;
import java.util.Map;
import com.sun.jdi.connect.Connector;
import com.sun.jdi.VirtualMachine;
import jde.debugger.DebuggeeProcess;
import java.io.IOException;
import com.sun.jdi.connect.IllegalConnectorArgumentsException;
import jde.debugger.Jdebug;
import jde.debugger.Debug;


/**
 * Attaches to an already running application through shared memory.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * attach_shmem  app_id name
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> The debugee vm has to have been launched with the right parameters.
 * See the <italic>Connection and Invocation</italic> section of the
 * JPDA documentation.
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.3 $
 */
public class AttachShmem extends DebugSessionCommand {
  
  protected void doCommand() throws JDEException {

    // the attaching connector...
    String connectSpec = null;
    connectSpec = "com.sun.jdi.SharedMemoryAttach";

    AttachingConnector connector = (AttachingConnector) getConnector(connectSpec);
    if (connector  == null) 
      throw new JDEException("No such connector is available: "+connectSpec);

    if (args.size() < 1)
      throw new JDEException("Missing name");

    try {
      Map argumentMap = connector.defaultArguments();

      Connector.Argument nameArg =
	(Connector.Argument)argumentMap.get("name");
      nameArg.setValue(args.remove(0).toString());

      VirtualMachine vm = connector.attach(argumentMap);


      // note that new process might raise a jdeexception.
      DebuggeeProcess proc = new DebuggeeProcess(procID, vm);

      if (procRegistry.processExists(procID)) {
	  proc.shutdown();
	  throw new JDEException("A process with the ID" + procID +
				 " already exists.");
      }

      procRegistry.addProcess(procID, proc);

      jde.signalCommandResult(procID, cmdID);

      jde.signal(procID, MESSAGE, "Attached VM (shmem) " + vm.description());

    } catch (IOException ex) {
      Debug.printIf(ex);
      throw new JDEException("Error attempting to attach to process via shared memory.");
    } catch (IllegalConnectorArgumentsException ex) {
      throw new JDEException("Illegal connector arguments for connector '"+connector);
    }
  }

  public Object clone() {return new AttachShmem();}
  
  
} // AttachShmem


/*
 * $Log: AttachShmem.java,v $
 * Revision 1.3  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.2  2000/02/02 05:58:12  paulk
 * Added command succeeded messages.
 *
 * Revision 1.1  2000/01/31 12:44:15  paulk
 * Attach existing application through shared memory.
 *
 */

// End of AttachShmem.java
