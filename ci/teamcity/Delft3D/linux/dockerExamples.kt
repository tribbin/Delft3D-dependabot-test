package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import Delft3D.linux.*
import Delft3D.template.*

import Trigger

object LinuxRunAllContainerExamples : BuildType({

    description = "Run all container example cases for fm/ and all/ merge-requests using Docker and Apptainer."

    templates(
        TemplateMergeRequest,
        TemplateDockerRegistry,
        TemplatePublishStatus,
        TemplateMonitorPerformance
    )

    name = "Run all container examples (Matrix)"
    buildNumberPattern = "%dep.${LinuxRuntimeContainers.id}.product%: %build.vcs.number%"

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }
    
    params {
        param("container_runtime", "")
    }
    
    steps {
        script {
            name = "Execute run_all_examples_container.sh with %container_runtime%"
            scriptContent = """
                cd ./examples/dflowfm/
                ./run-all-examples-container.sh --%container_runtime% --image "%dep.${LinuxRuntimeContainers.id}.runtime_container_image%"
            """.trimIndent()
        }
    }
  
    features {
        matrix {
            id = "container_matrix"
            param("container_runtime", listOf("docker", "apptainer").map { MatrixFeature.Value(it) })
        }
    }
  
    failureConditions {
        executionTimeoutMin = 180
        errorMessage = true
        failOnText {
            conditionType = BuildFailureOnText.ConditionType.CONTAINS
            pattern = "KILLED BY SIGNAL"
            failureMessage = "Bad termination of one of your application processes"
            reverse = false
        }
    }

    dependencies {
        dependency(Trigger) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
            }
        }
        dependency(LinuxRuntimeContainers) {
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})
