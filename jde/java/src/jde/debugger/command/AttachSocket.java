/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.4 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import com.sun.jdi.connect.AttachingConnector;
import java.util.Map;
import com.sun.jdi.connect.Connector;
import com.sun.jdi.VirtualMachine;
import jde.debugger.DebuggeeProcess;
import jde.debugger.Jdebug;
import java.io.IOException;
import jde.debugger.Debug;
import com.sun.jdi.connect.IllegalConnectorArgumentsException;


/**
 * Attaches to an already running application through a socket.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * attach_socket app_id -port p_value [-host h_value]
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
 * @version $Revision: 1.4 $
 */
public class AttachSocket extends DebugSessionCommand {

  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  protected void doCommand() throws JDEException {
 
    // the attaching connector...
    String connectSpec = null;
    connectSpec = "com.sun.jdi.SocketAttach";


    AttachingConnector connector = (AttachingConnector) getConnector(connectSpec);
    if (connector  == null) 
      throw new JDEException("No such connector is available: "+connectSpec);

    if (args.size() < 1)
      throw new JDEException("Missing arguments: specify at least the port");

    try {
      Map argumentMap = connector.defaultArguments();

      while ((args.size() > 0)
	     && args.get(0).toString().startsWith("-")) {
	String arg = args.remove(0).toString().toLowerCase();
	if (arg.equals("-host")) {
	  if (args.size() == 0)
	    throw new JDEException("Missing argument to 'host'");
	  String host = args.remove(0).toString();
	  Connector.Argument hostArg =
	    (Connector.Argument)argumentMap.get("hostname");
	  hostArg.setValue(host);
	} else if (arg.equals("-port")) {
	  if (args.size() == 0)
	    throw new JDEException("Missing argument to 'port'");
	  String port = args.remove(0).toString();
	  Connector.Argument portArg =
	    (Connector.Argument)argumentMap.get("port");
	  portArg.setValue(port);
	} else {
	  args.add(0, arg);
	  break;
	}
      }

      VirtualMachine vm = connector.attach(argumentMap);

    // note that new process might raise a jdeexception.
    DebuggeeProcess proc = new DebuggeeProcess(procID, vm);

    if (procRegistry.processExists(procID)) {
      proc.shutdown();
      throw new JDEException("A process with the ID " + procID +
			     " already exists.");
    }

    procRegistry.addProcess(procID, proc);

    jde.signalCommandResult(procID, cmdID);
    
    jde.signal(procID, MESSAGE, "Attached VM (socket) " + vm.description());

    } catch (IOException ex) {
      Debug.printIf(ex);
      throw new JDEException("I/O error occurred while attempting to attach process.");
    } catch (IllegalConnectorArgumentsException ex) {
      throw new JDEException("Illegal connector arguments for connector '"+connector);
    }

  }

  public Object clone() {return new AttachSocket();}
 
  
} // AttachSocket


/*
 * $Log: AttachSocket.java,v $
 * Revision 1.4  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.3  2000/08/09 03:43:07  paulk
 * Fixed bug where JDEBug would not attach to a process running on a remote host because it was setting the wrong connector argument (host instead of hostname). Thanks to Matthew Conway <matt_conway@i2.com>.
 *
 * Revision 1.2  2000/02/02 05:58:12  paulk
 * Added command succeeded messages.
 *
 * Revision 1.1  2000/01/31 12:45:08  paulk
 * Attach existing application through socket.
 *
 */

// End of AttachSocket.java
