package com.mysema.scalagen

import com.github.javaparser.ast._
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.comments._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.`type`._
import com.github.javaparser.ast.visitor.GenericVisitor
import java.util.{ArrayList, Collections}

import com.github.javaparser.ast.nodeTypes.NodeWithJavadoc
import com.mysema.scalagen.ast.BeginClosureExpr

/**
 * 
 */
abstract class ModifierVisitor[A] extends GenericVisitor[Node, A] {
  
  protected def filter[T <: Node](node: java.util.Optional[T], arg: A): T = {
    if (node.isPresent) node.get().accept(this, arg).asInstanceOf[T] else null
  }
  
  protected def filter[T <: Node](list: NodeList[T], arg: A): NodeList[T]  = {
    if (list == null) {
      null
    } else if (list.isEmpty) {
       new NodeList[T] 
    } else {
      //list.map(_.accept(this, arg).asInstanceOf[T]).filter(_ != null)
      val rv = new NodeList[T]()
      val it = list.iterator()
      while (it.hasNext) {
        val node = it.next().accept(this, arg).asInstanceOf[T]
        if (node != null) rv.add(node)
      }
      rv
    }    
  }

  def withCommentsFrom[T <: Node](origNode: Node, arg: A)(node: => T): T = {
    val newNode = node
    val comment = origNode match {
      case d: NodeWithJavadoc[_] => Option(javadocFor(d, arg))
      case _ => None
    }
    newNode.setComment(comment.getOrElse(origNode.getComment))
    origNode.getOrphanComments.foreach(newNode.addOrphanComment)

    newNode
  }

  def visitName(name: String, arg: A) = name

  def visit(n: AnnotationDeclaration, arg: A) : Node = withCommentsFrom(n, arg) {
    val rv = new AnnotationDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setMembers(filter(n.getMembers, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(n.getName)
    rv    
  }

  def visit(n: AnnotationMemberDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new AnnotationMemberDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setDefaultValue(filter(n.getDefaultValue, arg))
    rv.setComment(javadocFor(n, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(n.getName)
    rv.setType(filter(n.getType, arg))
    
    n
  }

  def visit(n: ArrayAccessExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ArrayAccessExpr()
    rv.setIndex(filter(n.getIndex, arg))
    rv.setName(filter(n.getName, arg))    
    rv
  }

  def visit(n: ArrayCreationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ArrayCreationExpr()    
    rv.setLevels(n.getLevels)
    rv.setInitializer(filter(n.getInitializer, arg))
    rv.setElementType(filter(n.getElementType, arg))   
    rv
  }

  def visit(n: ArrayInitializerExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ArrayInitializerExpr()
    rv.setValues(filter(n.getValues, arg))   
    rv
  }

  def visit(n: AssertStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new AssertStmt()
    rv.setCheck(filter(n.getCheck, arg))
    rv.setMessage(filter(n.getMessage, arg))    
    rv
  }

  def visit(n: AssignExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new AssignExpr()
    rv.setOperator(n.getOperator)
    rv.setTarget(filter(n.getTarget, arg))
    rv.setValue(filter(n.getValue, arg))    
    rv
  }

  def visit(n: BinaryExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new BinaryExpr()
    rv.setOperator(n.getOperator)
    rv.setLeft(filter(n.getLeft, arg))
    rv.setRight(filter(n.getRight, arg))    
    rv
  }

  def visit(n: BlockStmt, arg: A): Node = withCommentsFrom(n, arg) { new BlockStmt(filter(n.getStatements, arg)) }

  def visit(n: BooleanLiteralExpr, arg: A): Node = new BooleanLiteralExpr(n.getValue)

  def visit(n: BreakStmt, arg: A): Node = withCommentsFrom(n, arg) { new BreakStmt(n.getLabel) }

  def visit(n: CastExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new CastExpr(filter(n.getType, arg), filter(n.getExpression, arg))    
  }

  def visit(n: CatchClause, arg: A): Node = withCommentsFrom(n, arg) {
    new CatchClause(filter(n.getParameter, arg), filter(n.getBody, arg))
  }

  def visit(n: CharLiteralExpr, arg: A): Node = new CharLiteralExpr(n.getValue)

  def visit(n: ClassExpr, arg: A): Node = withCommentsFrom(n, arg) { new ClassExpr(filter(n.getType, arg)) }

  def visit(n: ClassOrInterfaceDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ClassOrInterfaceDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setExtendedTypes(filter(n.getExtendedTypes, arg))
    rv.setImplementedTypes(filter(n.getImplementedTypes, arg))
    rv.setInterface(n.isInterface)
    rv.setMembers(filter(n.getMembers, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(n.getName)
    rv.setTypeParameters(filter(n.getTypeParameters, arg))        
    rv
  }

  private def javadocFor(n: NodeWithJavadoc, arg: A) = {
    val filtered = filter(n.getJavaDoc, arg)
    if (filtered == null)
      null
    else
      new JavadocComment(filtered.getContent)
  }

  def visit(n: ClassOrInterfaceType, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ClassOrInterfaceType()
    rv.setName(n.getName)
    rv.setScope(filter(n.getScope, arg))
    rv.setTypeArguments(filter(n.getTypeArguments, arg))
    rv
  }

  def visit(n: CompilationUnit, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new CompilationUnit()
    rv.setPackageDeclaration(filter(n.getPackageDeclaration, arg))
    rv.setImports(filter(n.getImports, arg))
    rv.setTypes(filter(n.getTypes, arg))
    rv
  }

  def visit(n: ConditionalExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ConditionalExpr()
    rv.setCondition(filter(n.getCondition, arg))
    rv.setThenExpr(filter(n.getThenExpr, arg))
    rv.setElseExpr(filter(n.getElseExpr, arg))
    rv
  }

  def visit(n: ConstructorDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ConstructorDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setBody(filter(n.getBody, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(n.getName)
    rv.setParameters(filter(n.getParameters, arg))
    rv.setThrownExceptions(filter(n.getThrownExceptions, arg))
    rv.setTypeParameters(filter(n.getTypeParameters, arg))
    rv
  }

  def visit(n: ContinueStmt, arg: A): Node = withCommentsFrom(n, arg) { new ContinueStmt(n.getLabel.orElse(null)) }

  def visit(n: DoStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new DoStmt()
    rv.setBody(filter(n.getBody, arg))
    rv.setCondition(filter(n.getCondition, arg))
    rv
  }

  def visit(n: DoubleLiteralExpr, arg: A): Node = new DoubleLiteralExpr(n.getValue)

  def visit(n: EmptyStmt, arg: A): Node = new EmptyStmt()

  def visit(n: EnclosedExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new EnclosedExpr(filter(n.getInner, arg))
  }

  def visit(n: EnumConstantDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new EnumConstantDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setArguments(filter(n.getArguments, arg))
    rv.setClassBody(filter(n.getClassBody, arg))
    rv.setComment(javadocFor(n, arg))
    rv.setName(n.getName)
    rv
  }

  def visit(n: EnumDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new EnumDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setEntries(filter(n.getEntries, arg))
    rv.setImplementedTypes(filter(n.getImplementedTypes, arg))
    rv.setComment(javadocFor(n, arg))
    rv.setMembers(filter(n.getMembers, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(n.getName)    
    rv
  }

  def visit(n: ExplicitConstructorInvocationStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ExplicitConstructorInvocationStmt()
    rv.setArguments(filter(n.getArguments, arg))
    rv.setExpression(filter(n.getExpression, arg))
    rv.setThis(n.isThis)    
    rv.setTypeArguments(filter(n.getTypeArguments, arg))
    rv
  }

  def visit(n: ExpressionStmt, arg: A): Node = withCommentsFrom(n, arg) {
    new ExpressionStmt(filter(n.getExpression, arg))
  }

  def visit(n: FieldAccessExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new FieldAccessExpr(filter(n.getScope, arg), visitName(n.getField, arg))
  }

  def visit(n: FieldDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new FieldDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setModifiers(n.getModifiers)
    rv.setType(filter(n.getType, arg))
    rv.setVariables(filter(n.getVariables, arg))
    rv
  }

  def visit(n: ForeachStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ForeachStmt()
    rv.setVariable(filter(n.getVariable, arg))
    rv.setIterable(filter(n.getIterable, arg))
    rv.setBody(filter(n.getBody, arg))
    rv
  }

  def visit(n: ForStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ForStmt()
    rv.setInitialization(filter(n.getInitialization, arg))
    rv.setCompare(filter(n.getCompare, arg))
    rv.setUpdate(filter(n.getUpdate, arg))
    rv.setBody(filter(n.getBody, arg))
    rv
  }

  def visit(n: IfStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new IfStmt()
    rv.setCondition(filter(n.getCondition, arg))
    rv.setThenStmt(filter(n.getThenStmt, arg))
    rv.setElseStmt(filter(n.getElseStmt, arg))
    rv
  }

  def visit(n: ImportDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    new ImportDeclaration(n.getName, n.isStatic, n.isAsterisk)    
  }

  def visit(n: InitializerDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new InitializerDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setBody(filter(n.getBody, arg))
    rv.setStatic(n.isStatic)
    rv
  }

  def visit(n: InstanceOfExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new InstanceOfExpr()
    rv.setExpression(filter(n.getExpression, arg))
    rv.setType(filter(n.getType, arg))
    rv
  }

  def visit(n: IntegerLiteralExpr, arg: A): Node = new IntegerLiteralExpr(n.getValue)

  def visit(n: JavadocComment, arg: A): Node = new JavadocComment(n.getContent)

  def visit(n: LabeledStmt, arg: A): Node = new LabeledStmt(n.getLabel, filter(n.getStatement, arg))    

  def visit(n: LongLiteralExpr, arg: A): Node = new LongLiteralExpr(n.getValue)

  def visit(n: MarkerAnnotationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new MarkerAnnotationExpr(filter(n.getName, arg))
  } 

  def visit(n: MemberValuePair, arg: A): Node = withCommentsFrom(n, arg) {
    new MemberValuePair(n.getName, filter(n.getValue, arg))
  }

  def visit(n: MethodCallExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new MethodCallExpr()
    rv.setArguments(filter(n.getArguments, arg))
    rv.setName(n.getName)
    rv.setScope(filter(n.getScope, arg))
    rv.setTypeArguments(filter(n.getTypeArguments, arg))
    rv
  }

  def visit(n: MethodDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new MethodDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setBody(filter(n.getBody, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(n.getName)
    rv.setParameters(filter(n.getParameters, arg))
    rv.setThrownExceptions(filter(n.getThrownExceptions, arg))
    rv.setType(filter(n.getType, arg))
    rv.setTypeParameters(filter(n.getTypeParameters, arg))
    rv
  }

  def visit(n: NameExpr, arg: A): Node = withCommentsFrom(n, arg) {
    n match {
      case closure: BeginClosureExpr => closure
      case _ => new NameExpr(visitName(n.getName, arg))
    }
  }

  def visit(n: NormalAnnotationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new NormalAnnotationExpr()
    rv.setName(filter(n.getName, arg))
    rv.setPairs(filter(n.getPairs, arg))
    rv
  }

  def visit(n: NullLiteralExpr, arg: A): Node = new NullLiteralExpr()

  def visit(n: ObjectCreationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ObjectCreationExpr()
    rv.setAnonymousClassBody(filter(n.getAnonymousClassBody, arg))
    rv.setArguments(filter(n.getArguments, arg))
    rv.setScope(filter(n.getScope, arg))
    rv.setType(filter(n.getType, arg))
    rv.setTypeArguments(filter(n.getTypeArguments, arg))
    rv
  }

  def visit(n: PackageDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new PackageDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setName(filter(n.getName, arg))
    rv    
  }
  
  def visit(n: Parameter, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new Parameter()
    visit(n, rv, arg)
    rv.setType(filter(n.getType, arg))
    rv.setVarArgs(n.isVarArgs)
    rv
  }

  def visit(n: UnionType, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new UnionType()
    visit(n, rv, arg)
    rv.setElements(new UnionType(n.getElements.map(tpe => filter(tpe, arg))))
    rv
  }

  def visit(n: PrimitiveType, arg: A): Node = new PrimitiveType(n.getType)      

  def visit(n: NameExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new NameExpr()
    rv.setName(n.getName)
    rv
  }

  def visit(n: ReturnStmt, arg: A): Node = withCommentsFrom(n, arg) {
    new ReturnStmt(filter(n.getExpression, arg))
  }

  def visit(n: SingleMemberAnnotationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new SingleMemberAnnotationExpr(filter(n.getName, arg), filter(n.getMemberValue, arg))
  }

  def visit(n: StringLiteralExpr, arg: A): Node = new StringLiteralExpr(n.getValue)

  def visit(n: SuperExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new SuperExpr(filter(n.getClassExpr, arg))
  }

  def visit(n: SwitchEntryStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new SwitchEntryStmt()
    rv.setLabel(filter(n.getLabel, arg))
    rv.setStatements(filter(n.getStatements, arg))
    rv
  }

  def visit(n: SwitchStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new SwitchStmt()
    rv.setSelector(filter(n.getSelector, arg))
    rv.setEntries(filter(n.getEntries, arg))
    rv
  }

  def visit(n: SynchronizedStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new SynchronizedStmt() 
    rv.setExpression(filter(n.getExpression, arg))
    rv.setBody(filter(n.getBody, arg))
    rv
  }

  def visit(n: ThisExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new ThisExpr(filter(n.getClassExpr, arg))
  }

  def visit(n: ThrowStmt, arg: A): Node = withCommentsFrom(n, arg) {
    new ThrowStmt(filter(n.getExpression, arg))
  }

  def visit(n: TryStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new TryStmt()
    rv.setResources(filter(n.getResources, arg))
    rv.setTryBlock(filter(n.getTryBlock, arg))
    rv.setCatchClauses(filter(n.getCatchClauses, arg))
    rv.setFinallyBlock(filter(n.getFinallyBlock, arg))
    rv
  }

  def visit(n: TypeParameter, arg: A): Node = withCommentsFrom(n, arg) {
    new TypeParameter(n.getName, filter(n.getTypeBound, arg))
  }

  def visit(n: UnaryExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new UnaryExpr(filter(n.getExpression, arg), n.getOperator)    
  }

  def visit(n: VariableDeclarationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new VariableDeclarationExpr()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setModifiers(n.getModifiers)
    rv.setVariables(filter(n.getVariables, arg))
    rv
  }

  def visit(n: VariableDeclarator, arg: A): Node = withCommentsFrom(n, arg) {
    new VariableDeclarator(filter(n.getName, arg), filter(n.getInitializer, arg))
  }

  def visit(n: VoidType, arg: A): Node = new VoidType()

  def visit(n: WhileStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new WhileStmt()
    rv.setCondition(filter(n.getCondition, arg))
    rv.setBody(filter(n.getBody, arg))
    rv
  }

  def visit(n: WildcardType, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new WildcardType()
    rv.setExtendedTypes(filter(n.getExtendedTypes, arg))
    rv.setSuperTypes(filter(n.getSuperTypes, arg))
    rv
  }

  def visit(n: BlockComment, arg: A): Node = new BlockComment(n.getContent)

  def visit(n: LineComment, arg: A): Node = new LineComment(n.getContent)

  def visit(n: TypeExpr, arg: A): Node =
    new TypeExpr(
      n.getRange,
      filter(n.getType, arg)
    )

  def visit(n: MethodReferenceExpr, arg: A): Node =
    new MethodReferenceExpr(
      n.getRange,
      filter(n.getScope, arg),
      n.getTypeArguments,
      visitName(n.getIdentifier, arg)
    )
  def visit(n: LambdaExpr, arg: A): Node =
    new LambdaExpr(
      n.getRange,
      filter(n.getParameters, arg),
      filter(n.getBody, arg),
      n.isEnclosingParameters
    )

  def visit(n: UnknownType, arg: A): Node =
    new UnknownType()

  def visit(n: UnionType, arg: A): Node =
    new UnionType(filter(n.getElements, arg))

  def visit(n: IntersectionType, arg: A): Node =
    new IntersectionType(filter(n.getElements, arg))

}
