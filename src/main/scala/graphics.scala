package cp420.graphics

import org.lwjgl.*
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*

import scala.util.Using


/*@main*/ def run =
  glfwInit()

  glfwDefaultWindowHints()
  glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
  glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)

  val window = glfwCreateWindow(640, 480, "Hello, world", NULL, NULL)  
  
  glfwSetKeyCallback(window, (window, key, scanCode, action, mods) =>
    if key == GLFW_KEY_ESCAPE && action == GLFW_RELEASE then
      glfwSetWindowShouldClose(window, true)
  )

  glfwMakeContextCurrent(window)
//  glfwSwapInterval(1)
  glfwShowWindow(window)
      
  GL.createCapabilities()
  glClearColor(1f, 0f, 0f, 0f)

  glMatrixMode(GL_PROJECTION)
  glLoadIdentity()
  glOrtho(-1f, 1f, -1f, 1f, -1f, 1f)

  glMatrixMode(GL_MODELVIEW)
  glLoadIdentity()

  var angle = 0f
  var currentTime = glfwGetTime()
  val frameRate = 60

  while !glfwWindowShouldClose(window) do
    glClear(GL_COLOR_BUFFER_BIT)

    glPushMatrix()
    glRotatef(angle, 0f, 0f, 1f)

    glBegin(GL_TRIANGLES)
    glColor3f(1f, 0f, 0f)
    glVertex2f(-.5f, -.5f)
    glColor3f(0f, 1f, 0f)
    glVertex2f(.5f, -.5f)
    glColor3f(0f, 0f, 1f)
    glVertex2f(0f, .5f)
    glEnd()

    glPopMatrix()

    glfwSwapBuffers(window)
    glfwPollEvents()

    angle += 1f

  glfwDestroyWindow(window)
  glfwTerminate()

  println("hi")