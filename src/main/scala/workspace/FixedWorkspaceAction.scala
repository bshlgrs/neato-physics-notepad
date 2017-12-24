package workspace

import workspace.Workspace.VarId

trait WorkspaceAction


trait FixedWorkspaceAction extends WorkspaceAction

case class AddEqualityAction(varId1: VarId, varId2: VarId) extends FixedWorkspaceAction
case class RemoveEqualityAction(varId: VarId) extends FixedWorkspaceAction
case class AddExpressionAction(varId: VarId) extends FixedWorkspaceAction
case class DeleteExpressionAction(varId: VarId) extends FixedWorkspaceAction
case class RewriteExpressionAction(varId: VarId, varToRemoveId: VarId, equationToUseId: Int) extends FixedWorkspaceAction
case class AttachNumberAction(numberId: Int, varId: VarId) extends FixedWorkspaceAction
case class DetachNumberAction(numberId: Int) extends FixedWorkspaceAction
case class DeleteNumberAction(numberId: Int) extends FixedWorkspaceAction

trait FreeWorkspaceAction extends WorkspaceAction
case class AddEquationAction(equation: Equation) extends FreeWorkspaceAction
case class AddNumberAction(number: PhysicalNumber) extends FreeWorkspaceAction
