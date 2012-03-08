package jde.debugger;

import com.sun.jdi.connect.Connector;
import com.sun.jdi.connect.LaunchingConnector;
import com.sun.jdi.Bootstrap;
import com.sun.jdi.VirtualMachine;
import com.sun.jdi.connect.VMStartException;
import com.sun.jdi.connect.IllegalConnectorArgumentsException;
import com.sun.jdi.connect.AttachingConnector;
import com.sun.jdi.connect.ListeningConnector;
import java.io.BufferedReader;
import java.io.PrintWriter;
import java.util.Map;
import java.util.Collection;
import java.io.IOException;
import java.util.List;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Collections;
import java.util.ArrayList;
import java.util.Iterator;
import jde.debugger.command.DebugCommandFactory;

/**
 * Class of JDEbug debuggers.
 * <p>
 * This class defines methods for communicating with the JDE. It
 * maintains a list of active applications. It passes application
 * commands to the apps specified by the commands. 
 * <p>
 * See {@link Protocol Protocol class} for command/response formats and
 * {@link EventHandler EventHandler} for event set formats.
 * <p>
 * JDEbug responds to every command with either a result or error 
 * message.
 * <p>
 *
 * @author Amit Kumar
 * @since 0.1
 * @author Paul Kinnucan
 * @since 1.3
 */
public class Jdebug extends Thread implements Protocol {


  /********************************************************************
   * CONSTRUCTORS                                                     *
   ********************************************************************/

  protected Jdebug() {}

  public void init() throws IOException {
        
    jde.init();

  }

  /********************************************************************
   * METHODS                                                          *
   ********************************************************************/

  /**
   * Runs the debugger thread. This method reads and executes commands
   * from the JDE.
   */
  public void run() {

    List command = JDE.getJDE().nextCommand();
    while (command != null) {

      final Integer proc_id = Integer.valueOf(command.get(0).toString());
      final Integer cmd_id = Integer.valueOf(command.get(1).toString());
      final String cmd_name = command.get(2).toString().toLowerCase();
      final List arguments =  command.subList(3, command.size());

      try {
	Thread cmd = 
	  DebugCommandFactory.theFactory.createCommand(proc_id, cmd_id, cmd_name, arguments);
	cmd.start();
      }
      catch (JDEException ex) {
	jde.signal(proc_id, ERROR, "Error occurred while executing " + cmd_name +
		   ". Error: " + ex);
      }
    
      command = JDE.getJDE().nextCommand();
    } 
  }

  public static Jdebug getTheDebugger() {
    return theDebugger;
  }


  /********************************************************************
   * FIELDS                                                           *
   ********************************************************************/




  /**
   * The ID of jdebug. This is used by jde when issuing commands that
   * are not specific to any particular vm, for instance, 'quit', or
   * the command used to launch new application/vms.<br>
   * It is the Integer -1.
   */
  public static final Integer debuggerID = new Integer(-1); 

  public static Jdebug theDebugger = new Jdebug();

  private JDE jde = JDE.getJDE();

  private ProcessRegistry procRegistry = ProcessRegistry.getRegistry();



} // Jdebug

/*
 * $Log: Jdebug.java,v $
 * Revision 1.10  2001/08/14 05:15:01  paulk
 * Miscellaneous updates.
 *
 * Revision 1.9  2001/03/24 05:36:48  paulk
 * Updated to reflect reorganization of debuggee code.
 *
 * Revision 1.8  2000/10/20 04:18:29  paulk
 * *** empty log message ***
 *
 * Revision 1.7  2000/07/28 06:26:31  paulk
 * Committing all modified files.
 *
 * Revision 1.6  2000/02/14 06:25:34  paulk
 * Implemented workaround for JPDA bug that prevented setting of
 * breakpoints in inner classes.
 *
 * Revision 1.5  2000/01/31 12:41:39  paulk
 * * Continue converting commands from functional to OO implementation.
 *
 * Revision 1.4  2000/01/30 12:47:40  paulk
 * Changed to a singleton class. Implemented support for object-oriented
 * commands created by DebugCommandFactory. Reimplemented launch and
 * listen commands as object-oriented commands.
 *
 * Revision 1.3  2000/01/28 04:24:55  paulk
 * Threaded listen commands. Moved launch, attach, and listen commands
 * from Application to Jdebug class. Did general cleanup of Jdebug and
 * Application class, including using more specific names for some
 * variables, moving fields to the end of files, rewriting comments.
 *
 */


// End of Jdebug.java
