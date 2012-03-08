/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.6 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;

import com.sun.jdi.connect.LaunchingConnector;
import java.util.Map;
import com.sun.jdi.connect.Connector;
import java.util.Iterator;
import com.sun.jdi.VirtualMachine;
import java.io.IOException;
import com.sun.jdi.connect.IllegalConnectorArgumentsException;
import com.sun.jdi.connect.VMStartException;
import jde.debugger.DebuggeeProcess;
import jde.debugger.Jdebug;
import jde.debugger.Debug;
import jde.debugger.DebuggeeSIO;


/**
 * Launches an application.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * launch app_id [-use_executable javax] classname [args]
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.6 $
 */
public class LaunchApplication extends DebugSessionCommand {

  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {

    // this is the connector that launches a debuggee vm
    String connectSpec = "com.sun.jdi.CommandLineLaunch";

    // check if this kind of connector is, indeed,
    // available. if not, throw an exception.
    LaunchingConnector connector = (LaunchingConnector) getConnector(connectSpec);
    if (connector == null) 
      throw new JDEException("No such connector is available: "+connectSpec);


    // first set up the argument map. a table that describes the
    // different keys should be in the public jpda documentation.
    // internally, it is at
    // http://jbug/jbug/doc/conninv.html
    Map argumentMap = connector.defaultArguments();
	
    Connector.Argument mainArg =
      (Connector.Argument)argumentMap.get("main");

    // compose the command line
    String commandLine = "";
    String quote =((Connector.Argument)argumentMap.get("quote")).value();

    // check if there are special launch options we need to process
    if (args.size() == 0)
      throw new JDEException("Insufficient arguments");
	
    String executable = "java";
    // be careful with the loop here....
    while ((args.size() >0)
	   && args.get(0).toString().startsWith("-")) {
      String arg = args.remove(0).toString().toLowerCase();
      if (arg.equals("-vmexec")) {
	if (args.size() == 0)
	  throw new JDEException("Missing argument to 'use_executable'");
	executable = args.remove(0).toString();
	Connector.Argument vmexecArg =
	  (Connector.Argument)argumentMap.get("vmexec");
	vmexecArg.setValue(executable);
      }  
      else if (arg.equals("-home")) {
	if (args.size() == 0) 
	  throw new JDEException("Missing argument to 'home'");
	String home = args.remove(0).toString();
	Connector.Argument homeArg = (Connector.Argument) argumentMap.get("home");
	homeArg.setValue(home);
	continue;
      }
      else {
	args.add(0, arg);
	break;
      }
    }
	
    if (args.size() == 0)
      throw new JDEException("Missing arguments: no class specified?");
	
    // take care of spaces too! so quote everything.
    Iterator iterator = args.iterator();
    while(iterator.hasNext()) {
      // commandLine += quote + iterator.next() + quote + " ";
      String arg = (String)iterator.next();
      if (arg.equalsIgnoreCase("-classic")) {
	Connector.Argument optionsArg =
	  (Connector.Argument)argumentMap.get("options");
	String options = optionsArg.value();
	options = "-classic" + " " + options;
	optionsArg.setValue(options);
	jde.signal(procID, MESSAGE, "VM options: '" + options + "'");
      }
      else
	commandLine += quote + arg + quote + " ";
    }
    mainArg.setValue(commandLine);
	
    //	    signal(DEBUG, "Command line: '"+executable+" "+commandLine+"'");

    VirtualMachine vm = null;

    try {
      vm = connector.launch(argumentMap);
      jde.signal(procID, MESSAGE, "Launched VM " + vm.description());
    } catch (IOException ex) {
      Debug.printIf(ex);
      throw new JDEException("Unable to launch: " + ex.toString().replace('\\','/'));
    } catch (IllegalConnectorArgumentsException ex) {
      throw new JDEException("Invalid or inconsistent connector arguments for connector '"+connector+"'");
    } catch (VMStartException ex) {
      // dumpFailedAppStreams(ex.process());
      throw new JDEException(ex.getMessage().toString().replace('\\','/'));
    }

    // note that new debuggee process might raise a jdeexception.
    DebuggeeProcess proc = new DebuggeeProcess(procID, vm);

    if (procRegistry.processExists(procID)) {
	proc.shutdown();
	throw new JDEException("An application with the same ID exists.");
      }

    procRegistry.addProcess(procID, proc);
    
    DebuggeeSIO procSIO = proc.getSIO();

    // Create a socket for connecting the application's standard I/O
    // to Emacs and a thread that waits for Emacs to connect to the socket.
    int port = procSIO.initConnect(cmdID);

    // Tell Emacs the number of the standard I/O socket.
//    jde.signalCommandResult(procID, cmdID, new Integer(port));
  }

  public Object clone() {return new LaunchApplication();}
  
} // LaunchApplication


/*
 * $Log: LaunchApplication.java,v $
 * Revision 1.6  2001/05/24 02:50:06  paulk
 * Moved  jde.signalCommandResult(....,
 * port) from LaunchApplication.java  into the same thread that creates a
 * server SIO socket to make sure that Emacs connects to this socket after its
 * creation. Thanks to "Eugene Gavrilov" <eag99@mail.ru>.
 *
 * Revision 1.5  2001/03/24 05:42:37  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.4  2000/07/28 06:27:02  paulk
 * Committing all modified files.
 *
 * Revision 1.3  2000/03/03 07:45:05  paulk
 * Replaced backslashes with forward slashes in launch error messages.
 *
 * Revision 1.2  2000/01/31 12:41:45  paulk
 * * Continue converting commands from functional to OO implementation.
 *
 * Revision 1.1  2000/01/30 12:39:50  paulk
 * Defines command to launch debuggee application.
 *
 */

// End of LaunchApplication.java
