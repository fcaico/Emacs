/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.2 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import com.sun.jdi.connect.ListeningConnector;
import java.util.Map;
import com.sun.jdi.connect.Connector;
import com.sun.jdi.VirtualMachine;
import jde.debugger.DebuggeeProcess;
import com.sun.jdi.connect.IllegalConnectorArgumentsException;
import jde.debugger.Jdebug;
import java.io.IOException;
import jde.debugger.Debug;


/**
 * Listens on a socket for a debuggee application 
 * requesting debug services.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * listen_socket app_id port 
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 */
public class ListenSocket extends DebugSessionCommand {
  
  public ListenSocket() {
            
  }

  protected void doCommand() throws JDEException {

    if (args.size() < 1)
      throw new JDEException("Missing name");

    final String address = args.remove(0).toString();
    
    String connectSpec = "com.sun.jdi.SocketListen";

    final ListeningConnector connector = (ListeningConnector) getConnector(connectSpec);

    if (connector == null) 
      throw new JDEException("No such connector is available: "+connectSpec);
	
    Thread thread = new Thread("Listen on socket.") {

	public void run()  {
	  try {

	    Map argumentMap = connector.defaultArguments();

	    Connector.Argument portArg =
	      (Connector.Argument)argumentMap.get("port");
	    portArg.setValue(address);

	    jde.signalCommandResult(procID, cmdID);
	    jde.signal(procID, MESSAGE, "Listening at socket address: " + address);
	    connector.startListening(argumentMap);
	    VirtualMachine vm = connector.accept(argumentMap);
	    connector.stopListening(argumentMap);

	    // note that new process might raise a jdeexception.
	    DebuggeeProcess proc = new DebuggeeProcess(procID, vm);

 
	    if (procRegistry.processExists(procID)) {
	      proc.shutdown();
	      jde.signal(procID, MESSAGE, "Error: An application with the same ID exists.");
	    }

	    procRegistry.addProcess(procID, proc);
	

	    jde.signal(procID, MESSAGE, "Attached VM (socket) " + vm.description());
    

	  } catch (IOException ex) {
	    Debug.printIf(ex);
	    jde.signal(procID, MESSAGE, "Error occured listening on socket.");
	  } catch(IllegalConnectorArgumentsException ex) {
	    jde.signal(procID, MESSAGE, 
		       "Illegal connector arguments for connector '"+connector);
	  } 
	}
      };

    thread.start(); 	

  }

  public Object clone() {return new ListenSocket();}
 
  
} // ListenSocket


/*
 * $Log: ListenSocket.java,v $
 * Revision 1.2  2001/03/24 05:42:37  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.1  2000/01/30 12:45:18  paulk
 * Defines command to listen on a socket for a debuggee application
 * requiring debugger services.
 *
 */

// End of ListenSocket.java
