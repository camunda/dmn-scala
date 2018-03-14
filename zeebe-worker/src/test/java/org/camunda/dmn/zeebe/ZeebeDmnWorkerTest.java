package org.camunda.dmn.zeebe;

import static org.assertj.core.api.Assertions.assertThat;

import io.zeebe.client.ZeebeClient;
import io.zeebe.client.event.*;
import io.zeebe.model.bpmn.Bpmn;
import io.zeebe.model.bpmn.instance.WorkflowDefinition;
import io.zeebe.test.ZeebeTestRule;
import org.junit.*;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.rules.SpringClassRule;
import org.springframework.test.context.junit4.rules.SpringMethodRule;

@SpringBootTest
public class ZeebeDmnWorkerTest
{
    @Rule
    public final ZeebeTestRule testRule = new ZeebeTestRule();

    @ClassRule
    public static final SpringClassRule springClassRule = new SpringClassRule();

    @Rule
    public final SpringMethodRule springMethodRule = new SpringMethodRule();

    private ZeebeClient client;
    private String topic;

    @Before
    public void deploy()
    {
        final WorkflowDefinition workflow = Bpmn.createExecutableWorkflow("process")
            .startEvent()
            .serviceTask("dmn-task", t -> t
                         .taskType("DMN")
                         .taskHeader("decisionRef", "discount"))
            .endEvent()
            .done();

        client = testRule.getClient();
        topic = testRule.getDefaultTopic();

        client.workflows().deploy(topic)
                .addWorkflowModel(workflow, "process.bpmn")
                .execute();

        // util to get the task result
        testRule.getClient()
            .topics()
            .newSubscription(topic)
            .name("test")
            .taskEventHandler(new TaskCompleteEventHandler())
            .open();
    }

    @Test
    public void shouldReturnDecisionResult()
    {
        final WorkflowInstanceEvent workflowInstance = client.workflows().create(topic)
            .bpmnProcessId("process")
            .latestVersion()
            .payload("{\"customer\":\"Business\",\"orderSize\":15}")
            .execute();

        testRule.waitUntilWorkflowInstanceCompleted(workflowInstance.getWorkflowInstanceKey());

        assertThat(TaskCompleteEventHandler.payload).isEqualTo("{\"result\":0.15}");
    }

    @Test
    public void shouldReturnNilResult()
    {
        final WorkflowInstanceEvent workflowInstance = client.workflows().create(topic)
            .bpmnProcessId("process")
            .latestVersion()
            .payload("{\"customer\":\"VIP\",\"orderSize\":100}")
            .execute();

        testRule.waitUntilWorkflowInstanceCompleted(workflowInstance.getWorkflowInstanceKey());

        assertThat(TaskCompleteEventHandler.payload).isEqualTo("{\"result\":null}");
    }

    private static class TaskCompleteEventHandler implements TaskEventHandler
    {
        public static String payload;

        @Override
        public void handle(TaskEvent event) throws Exception
        {
            if (event.getState().equals("COMPLETED"))
            {
                payload = event.getPayload();
            }
        }
    }


}
