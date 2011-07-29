package org.OpenNI.Samples.UserTracker

import java.awt.event.KeyEvent
import java.awt.event.KeyListener
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent

import javax.swing.JFrame

object UserTrackerApplication extends App {
    val tracker: UserTracker = new UserTracker()
    var shouldRun = true

    val frame = new JFrame("OpenNI User Tracker")
    frame.addWindowListener(new WindowAdapter() {
        override def windowClosing(e: WindowEvent) { System.exit(0) }
    })
    frame.addKeyListener(new KeyListener() {
        override def keyTyped(arg0: KeyEvent) { }
        override def keyReleased(arg0: KeyEvent) { }
        override def keyPressed(arg0: KeyEvent) { 
            arg0.getKeyCode match {
                case KeyEvent.VK_ESCAPE => shouldRun = false 
                case KeyEvent.VK_B => tracker.drawBackground = !tracker.drawBackground
                case KeyEvent.VK_X => tracker.drawPixels = !tracker.drawPixels
                case KeyEvent.VK_S => tracker.drawSkeleton = !tracker.drawSkeleton
                case KeyEvent.VK_I => tracker.printID = !tracker.printID
                case KeyEvent.VK_L => tracker.printState = !tracker.printState
                case _ =>
            }
        }
    })

    frame.add("Center", tracker)
    frame.pack()
    frame.setVisible(true)

    while(shouldRun) {
        tracker.updateDepth()
        tracker.repaint()
    }
    frame.dispose()    
}
