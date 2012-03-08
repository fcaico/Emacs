/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.3 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import com.sun.jdi.connect.ListeningConnector;
import java.util.Map;
import com.sun.jdi.connect.Connector;
import jde.debugger.Jdebug;
import com.sun.jdi.VirtualMachine;
import jde.debugger.DebuggeeProcess;
import java.io.IOException;
import jde.debugger.Debug;
import com.sun.jdi.connect.IllegalConnectorArgumentsException;


/**
 *  Listens in shared memory for a debuggee vm requesting debug
 *  services.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * listen_shmem  app_id name
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.3 $
 */
public class ListenShmem extends DebugSessionCommand {
  
  protected void doCommand() throws JDEException {

    if (args.size() < 1)
      throw new JDEException("Missing name");

    final String address = args.remove(0).toString();
    
    String connectSpec = "com.sun.jdi.SharedMemoryListen";

    final ListeningConnector connector = (ListeningConnector) getConnector(connectSpec);

    if (connector == null) 
      throw new JDEException("No such connector is available: "+connectSpec);
	
    Thread thread = new Thread("Listen on shared memory channel.") {

	public void run()  {

	  try {
	    Map argumentMap = connector.defaultArguments();

	    Connector.Argument nameArg =
	      (Connector.Argument)argumentMap.get("name");
	    nameArg.setValue(address);
	    
	    jde.signalCommandResult(procID, cmdID);
	    jde.signal(procID, MESSAGE, 
		       "Listening at shared memory address: " + address);
	    connector.startListening(argumentMap);
	    VirtualMachine vm = connector.accept(argumentMap);
	    connector.stopListening(argumentMap);

	    // note that new App might raise a jdeexception.
	    DebuggeeProcess proc = new DebuggeeProcess(procID, vm);

 
	    if (procRegistry.processExists(procID)) {
		proc.shutdown();
		jde.signal(procID, MESSAGE, "An application with the same ID already exists.");
	    }
	    
	    procRegistry.addProcess(procID, proc);
	    
	   

	    jde.signal(procID, MESSAGE, "Attached VM (shmem) " + vm.description());
    
	    
	  } catch (IOException ex) {
	    Debug.printIf(ex);
	    jde.signal(procID, MESSAGE, 
		       "I/O error occured while listening at shared memory address:" 
		       + address);
	  } catch (IllegalConnectorArgumentsException ex) {
	    jde.signal(procID, MESSAGE, 
		       "Illegal argument error occurred while listening " + 
		       "at shared memory address: " + address);
	  }	    

	}
      };

    thread.start(); 	

  }

  public Object clone() {return new ListenShmem();}
  
} // ListenShmem


/*
 * $Log: ListenShmem.java,v $
 * Revision 1.3  2001/03/24 05:42:37  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.2  2000/04/10 05:44:54  paulk
 * Corrected spelling error in message.
 *
 * Revision 1.1  2000/01/30 12:42:19  paulk
 * Defines command to attach debugger to an existing application through
 * shared memory.
 *
 */

// End of ListenShmem.java
