package org.OpenNI.Samples.UserTracker

import org.OpenNI._

import java.nio.ShortBuffer
import java.util.HashMap
import java.awt._
import java.awt.color.ColorSpace
import java.awt.image._

class UserTracker extends java.awt.Component {
    val serialVersionUID = 1L
    var scriptNode: OutArg[ScriptNode] = null
    var context: Context = null
    var depthGen: DepthGenerator = null
    var userGen: UserGenerator = null
    var skeletonCap: SkeletonCapability = null
    var poseDetectionCap: PoseDetectionCapability = null
    var imgbytes: Array[Byte] = null
    var histogram = new Array[Float](10000)
    var calibPose:String = null
    var joints = new HashMap[Int, HashMap[SkeletonJoint, SkeletonJointPosition]]

    var drawBackground = true
    var drawPixels = true
    var drawSkeleton = true
    var printID = true
    var printState = true
    
    var bimg: BufferedImage = null
    
    val CONFIG_FILE = "../../../../Data/SamplesConfig.xml"

    try {
        scriptNode = new OutArg[ScriptNode]
        context = Context.createFromXmlFile(CONFIG_FILE, scriptNode)
        
        depthGen = DepthGenerator.create(context)
        val depthMD = depthGen.getMetaData
        
        setSize(depthMD.getFullXRes, depthMD.getFullYRes)        
        imgbytes = new Array[Byte](getWidth*getHeight*3)
        
        userGen = UserGenerator.create(context)
        skeletonCap = userGen.getSkeletonCapability
        poseDetectionCap = userGen.getPoseDetectionCapability
        
        userGen.getNewUserEvent().addObserver(new NewUserObserver())
        userGen.getLostUserEvent().addObserver(new LostUserObserver())
        skeletonCap.getCalibrationCompleteEvent().addObserver(new CalibrationCompleteObserver())
        poseDetectionCap.getPoseDetectedEvent().addObserver(new PoseDetectedObserver())
        
        calibPose = skeletonCap.getSkeletonCalibrationPose()
        
        skeletonCap.setSkeletonProfile(SkeletonProfile.ALL)
        
        context.startGeneratingAll()
    } catch { 
        case e:GeneralException => 
            e.printStackTrace()
            System.exit(1)
    }

    class NewUserObserver extends IObserver[UserEventArgs] {
        override def update(observable: IObservable[UserEventArgs], args: UserEventArgs) = {
            println("New user: " + args.getId)
            try {
                poseDetectionCap.StartPoseDetection(calibPose, args.getId)
            } catch {
                case e:StatusException => e.printStackTrace()
            }
        }
    }
    class LostUserObserver extends IObserver[UserEventArgs] {
        override def update(observable: IObservable[UserEventArgs], args: UserEventArgs) = {
            println("Lost user: " + args.getId)
            joints.remove(args.getId)
        }
    }
    class CalibrationCompleteObserver extends IObserver[CalibrationProgressEventArgs] {
        override def update(observable: IObservable[CalibrationProgressEventArgs], args: CalibrationProgressEventArgs) = {
            println("Calibration complete: " + args.getStatus)
            try {
                if(args.getStatus == CalibrationProgressStatus.OK) {
                    println("starting tracking " + args.getUser)
                        skeletonCap.startTracking(args.getUser)
                        joints.put(args.getUser, new HashMap[SkeletonJoint, SkeletonJointPosition]())
                } else {
                    poseDetectionCap.StartPoseDetection(calibPose, args.getUser)
                }
            } catch {
                case e:StatusException => e.printStackTrace()
            }
        }
    }
    class PoseDetectedObserver extends IObserver[PoseDetectionEventArgs] {
        override def update(observable: IObservable[PoseDetectionEventArgs], args: PoseDetectionEventArgs) {
            println("Pose " + args.getPose + " detected for " + args.getUser)
            try {
                poseDetectionCap.StopPoseDetection(args.getUser)
                skeletonCap.requestSkeletonCalibration(args.getUser, true)
            } catch {
                case e:StatusException => e.printStackTrace()
            }
        }
    }
   
    def calcHist(depth: ShortBuffer) = {
        // reset
        for(i <- 0 until histogram.length) histogram(i) = 0
        
        depth.rewind()

        var points = 0
        while(depth.remaining > 0) {
            val depthVal = depth.get
            if(depthVal != 0) {
                histogram(depthVal) += 1
                points += 1
            }
        }
        
        for(i <- 1 until histogram.length) histogram(i) += histogram(i-1)

        if(points > 0) for(i <- 1 until histogram.length) histogram(i) = 1.0f - (histogram(i) / points.toFloat)
    }

    def updateDepth() = {
        try {
            context.waitAnyUpdateAll()

            val depthMD = depthGen.getMetaData
            val sceneMD = userGen.getUserPixels(0)

            val scene = sceneMD.getData.createShortBuffer
            val depth = depthMD.getData.createShortBuffer
            calcHist(depth)
            depth.rewind()
            
            while(depth.remaining > 0) {
                val pos = depth.position
                val pixel = depth.get
                val user = scene.get
                
                imgbytes(3*pos) = 0
                imgbytes(3*pos+1) = 0
                imgbytes(3*pos+2) = 0

                if(drawBackground || pixel != 0) {
                    var colorID = user % (colors.length-1)
                    if(user == 0) colorID = colors.length-1
                    if(pixel != 0) {
                        val histValue = histogram(pixel)
                        imgbytes(3*pos) = (histValue*colors(colorID).getRed).toByte
                        imgbytes(3*pos+1) = (histValue*colors(colorID).getGreen).toByte
                        imgbytes(3*pos+2) = (histValue*colors(colorID).getBlue).toByte
                    }
                }
            }
        } catch {
            case e:GeneralException => e.printStackTrace()
        }
    }

    override def getPreferredSize:Dimension = return new Dimension(getWidth, getHeight)

    val colors = Array[Color](Color.RED, Color.BLUE, Color.CYAN, Color.GREEN, Color.MAGENTA, Color.PINK, Color.YELLOW, Color.WHITE)
    
    @throws(classOf[StatusException])
    def getJoint(user: Int, joint: SkeletonJoint) {
        val pos = skeletonCap.getSkeletonJointPosition(user, joint)
        if(pos.getPosition.getZ != 0) 
            joints.get(user).put(joint, new SkeletonJointPosition(depthGen.convertRealWorldToProjective(pos.getPosition), pos.getConfidence))
        else
            joints.get(user).put(joint, new SkeletonJointPosition(new Point3D, 0))
    }
    
    @throws(classOf[StatusException])
    def getJoints(user: Int) {
        import SkeletonJoint._
        getJoint(user, HEAD)
        getJoint(user, NECK)
        
        getJoint(user, LEFT_SHOULDER)
        getJoint(user, LEFT_ELBOW)
        getJoint(user, LEFT_HAND)
        
        getJoint(user, RIGHT_SHOULDER)
        getJoint(user, RIGHT_ELBOW)
        getJoint(user, RIGHT_HAND)

        getJoint(user, TORSO)

        getJoint(user, LEFT_HIP)
        getJoint(user, LEFT_KNEE)
        getJoint(user, LEFT_FOOT)

        getJoint(user, RIGHT_HIP)
        getJoint(user, RIGHT_KNEE)
        getJoint(user, RIGHT_FOOT)
    }
    
    def drawLine(g: Graphics, userJoints: HashMap[SkeletonJoint, SkeletonJointPosition], joint1: SkeletonJoint, joint2: SkeletonJoint) {
        val (userJoint1, userJoint2) = (userJoints.get(joint1), userJoints.get(joint2))
        if(userJoint1.getConfidence == 0 || userJoint2.getConfidence == 0) return;

        val (pos1, pos2) = (userJoint1.getPosition, userJoint2.getPosition)
        g.drawLine(pos1.getX.toInt, pos1.getY.toInt, pos2.getX.toInt, pos2.getY.toInt)
    }
    
    @throws(classOf[StatusException])
    def drawSkeleton(g: Graphics, user: Int) {
        getJoints(user)
        val userJoints = joints.get(user)

        import SkeletonJoint._
        drawLine(g, userJoints, HEAD, NECK)

        drawLine(g, userJoints, LEFT_SHOULDER, TORSO)
        drawLine(g, userJoints, RIGHT_SHOULDER, TORSO)

        drawLine(g, userJoints, NECK, LEFT_SHOULDER)
        drawLine(g, userJoints, LEFT_SHOULDER, LEFT_ELBOW)
        drawLine(g, userJoints, LEFT_ELBOW, LEFT_HAND)

        drawLine(g, userJoints, NECK, RIGHT_SHOULDER)
        drawLine(g, userJoints, RIGHT_SHOULDER, RIGHT_ELBOW)
        drawLine(g, userJoints, RIGHT_ELBOW, RIGHT_HAND)

        drawLine(g, userJoints, LEFT_HIP, TORSO)
        drawLine(g, userJoints, RIGHT_HIP, TORSO)
        drawLine(g, userJoints, LEFT_HIP, RIGHT_HIP)

        drawLine(g, userJoints, LEFT_HIP, LEFT_KNEE)
        drawLine(g, userJoints, LEFT_KNEE, LEFT_FOOT)

        drawLine(g, userJoints, RIGHT_HIP, RIGHT_KNEE)
        drawLine(g, userJoints, RIGHT_KNEE, RIGHT_FOOT)
    }
    
    override def paint(g: Graphics) {
        if(drawPixels) {
            val dataBuffer = new DataBufferByte(imgbytes, getWidth*getHeight*3)
            val raster = Raster.createInterleavedRaster(dataBuffer, getWidth, getHeight, getWidth*3, 3, Array[Int](0, 1, 2), null)
            val colorModel = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB), Array[Int](8, 8, 8), false, false, Transparency.OPAQUE, DataBuffer.TYPE_BYTE)
            bimg = new BufferedImage(colorModel, raster, false, null)
            g.drawImage(bimg, 0, 0, null)
        }
        
        try {
            val users = userGen.getUsers
            for(i <- 0 until users.length) {
                var c = colors(users(i)%colors.length)
                c = new Color(255-c.getRed, 255-c.getGreen, 255-c.getBlue)

                g.setColor(c)
                if(drawSkeleton && skeletonCap.isSkeletonTracking(users(i))) drawSkeleton(g, users(i))
                
                if (printID) {
                    val centerOfMass = depthGen.convertRealWorldToProjective(userGen.getUserCoM(users(i)))
                    var label:String = null
                    if(!printState) {
                        label = new String(""+users(i))
                    } else if(skeletonCap.isSkeletonTracking(users(i))) {
                        // Tracking
                        label = new String(users(i) + " - Tracking")
                    } else if(skeletonCap.isSkeletonCalibrating(users(i))) {
                        // Calibrating
                        label = new String(users(i) + " - Calibrating")
                    } else {
                        // Nothing
                        label = new String(users(i) + " - Looking for pose (" + calibPose + ")")
                    }

                    g.drawString(label, centerOfMass.getX.toInt, centerOfMass.getY.toInt)
                }
            }
        } catch {
            case e:StatusException => e.printStackTrace()
        }
    }
}

