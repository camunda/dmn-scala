# DMN engine - Camunda Plugin

A process engine plugin which replaces the [Camunda DMN engine](https://github.com/camunda/camunda-engine-dmn).

The DMN engine is used when a decision is evaluated via a Business Rule Task or the Decision Service. 

Current limitation:
* no history is written while evaluation

## How to use it?

**Using Camunda Tomcat Distribution**

Download the [plugin JAR](https://github.com/camunda/dmn-scala/releases) _(dmn-engine-camunda-plugin-${version}.jar)_. 

Copy the JAR in the tomcat lib folder (_server/apache-tomcat-8.0.47/lib/_).

Add the plugin to the process engine configuration (_conf/bpm-platform.xml_).

```xml
<process-engine name="default">
    
    <plugins>
    
      <plugin>
        <class>org.camunda.dmn.camunda.plugin.CamundaDmnEnginePlugin</class>
      </plugin>

    </plugins>

</process-engine>
```

Start the Tomcat server.

**Using Camunda Embedded Engine**

Add the plugin including the DMN engine to your project by copying the [jar file](https://github.com/camunda/feel-scala/releases) _(camunda-plugin-${version}.jar)_ or adding the project as dependency.

```xml
<dependency>
  <groupId>org.camunda.bpm.extension.dmn.scala</groupId>
  <artifactId>camunda-plugin</artifactId>
  <version>${version}</version>
</dependency>
```

Then, add the plugin in your process engine configuration.

```xml
<bean id="processEngineConfiguration" class="org.camunda.bpm.engine.impl.cfg.StandaloneProcessEngineConfiguration">
  
  <property name="processEnginePlugins">
    <list>
      <bean class="org.camunda.dmn.camunda.plugin.CamundaDmnEnginePlugin" />
    </list>
  </property>
    
</bean>
```

**Using Camunda Spring Boot Starter**

Add a configuration bean with the process engine plugin to your project.

```java
@Configuration
public class BpmPlatformConfiguration {

  @Bean
  public static ProcessEnginePlugin feelScalaPlugin() {
    return new CamundaDmnEnginePlugin();
  }
}
```

## How to build it?

You can build the project with [SBT](http://www.scala-sbt.org) or [Maven](http://maven.apache.org).

### Using SBT

In the root directory:


Run the tests with
```
sbt camundaPlugin/test
```

Build the jar including all dependencies with
```
sbt camundaPlugin/assembly
```

### Using Maven

Run the tests with
```
mvn test
```

Build the jar including all dependencies with
```
mvn install
```