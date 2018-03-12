import org.camunda.dmn.rest._
import org.scalatra._
import javax.servlet.ServletContext
import org.camunda.dmn.standalone.StandaloneEngine

class ScalatraBootstrap extends LifeCycle {
  
  override def init(context: ServletContext) 
  {
    // load configuration
    val repo = System.getProperty("dmn.repo", "dmn-repo")
    // create DMN engine
    val engine = StandaloneEngine.fileSystemRepository(repo)
    
    context.mount(new DmnEngineRestServlet(engine), "/*")
  }
}
