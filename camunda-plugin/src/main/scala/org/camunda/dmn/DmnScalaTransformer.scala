package org.camunda.dmn

import org.camunda.bpm.dmn.engine.impl.spi.transform.DmnTransformer
import org.camunda.bpm.dmn.engine.impl.spi.transform.DmnTransform
import java.io.InputStream
import java.io.File
import java.io.FileInputStream
import org.camunda.bpm.model.dmn.DmnModelInstance
import java.util.{List => JList}
import org.camunda.bpm.dmn.engine.impl.spi.transform.DmnTransformListener
import org.camunda.bpm.dmn.engine.impl.spi.transform.DmnElementTransformHandlerRegistry
import org.camunda.bpm.dmn.engine.impl.spi.`type`.DmnDataTypeTransformerRegistry
import org.camunda.bpm.dmn.engine.impl.spi.hitpolicy.DmnHitPolicyHandlerRegistry

class DmnScalaTransformer(dmnEngine: CamundaDmnEngine) extends DmnTransformer {
  
  override def createTransform = new DmnScalaTransform(dmnEngine)
  
  ///// not supported
  
  override def getTransformFactory = throw new UnsupportedOperationException
 
  override def getTransformListeners = throw new UnsupportedOperationException 
  
  override def setTransformListeners(transformListeners: JList[DmnTransformListener]) = throw new UnsupportedOperationException 
  
  override def transformListeners(transformListeners: JList[DmnTransformListener]) = throw new UnsupportedOperationException 
  
  override def getElementTransformHandlerRegistry = throw new UnsupportedOperationException 
  
  override def setElementTransformHandlerRegistry(registry: DmnElementTransformHandlerRegistry) = throw new UnsupportedOperationException 
  
  override def elementTransformHandlerRegistry(registry: DmnElementTransformHandlerRegistry) = throw new UnsupportedOperationException 
  
  override def getDataTypeTransformerRegistry = throw new UnsupportedOperationException 
  
  override def setDataTypeTransformerRegistry(registry: DmnDataTypeTransformerRegistry) = throw new UnsupportedOperationException 
  
  override def dataTypeTransformerRegistry(registry: DmnDataTypeTransformerRegistry) = throw new UnsupportedOperationException 
  
  override def getHitPolicyHandlerRegistry = throw new UnsupportedOperationException 
  
  override def setHitPolicyHandlerRegistry(registry: DmnHitPolicyHandlerRegistry) = throw new UnsupportedOperationException 
  
  override def hitPolicyHandlerRegistry(registry: DmnHitPolicyHandlerRegistry) = throw new UnsupportedOperationException 
  
}