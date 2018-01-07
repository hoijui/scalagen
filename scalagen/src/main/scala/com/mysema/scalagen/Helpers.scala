/*
 * Copyright (C) 2011, Mysema Ltd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.mysema.scalagen

import java.util
import java.util.EnumSet

import com.github.javaparser.ast.Modifier
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.nodeTypes.NodeWithModifiers

/**
 * Common helper methods for transformers and ScalaDumpVisitor
 */
trait Helpers {
  import Types._
  
  val PRIVATE = Modifier.PRIVATE
  val PROPERTY = 0x00001000
  val LAZY     = 0x00002000
  val OBJECT   = 0x00004000
  val IMPLICIT = 0x00008000  
  
  implicit def toRichModifiers(i: util.EnumSet[Modifier]) = new RichModifiers(i)
  
  class RichModifiers(i: util.EnumSet[Modifier]) {
    def isAbstract = i.contains(Modifier.ABSTRACT)
    def isFinal = i.contains(Modifier.FINAL)
	  // TODO ???
    def isImplicit = false
	  // TODO ???
    def isLazy = false
    def isNative = i.contains(Modifier.NATIVE)
	  // TODO ???
    def isObject = false
    def isPrivate = i.contains(Modifier.NATIVE)
    def isProtected = i.contains(Modifier.NATIVE)
	  // TODO not sure I'm doing the right thing here. It looks like the modifiers are being abused with Scala specific flags?
    def isProperty = false
    def isPublic = i.contains(Modifier.NATIVE)
    def isStatic = ModifierSet.isStatic(i)
    def isStrictfp = ModifierSet.isStrictfp(i)
    def isSynchronized = ModifierSet.isSynchronized(i)
    def isTransient = ModifierSet.isTransient(i)
    def isVolatile = ModifierSet.isVolatile(i)
    def hasModifier(mod: Int) = ModifierSet.hasModifier(i,mod)
    def addModifier(mod: Int) = ModifierSet.addModifier(i,mod)
    def removeModifier(mod: Int) = ModifierSet.removeModifier(i,mod)    
  }  
  
  type WithModifiers = { def getModifiers(): Int ; def setModifiers(v: Int): Unit }
  
  implicit def toRichWithModifiers(wm: WithModifiers) = new RichWithModifiers(wm)
  
  class RichWithModifiers(wm: WithModifiers) {
    def addModifier(mod: Int): RichWithModifiers = {
      wm.setModifiers(ModifierSet.addModifier(wm.getModifiers, mod))
      this
    } 
    def removeModifier(mod: Int): RichWithModifiers = {
      wm.setModifiers(ModifierSet.removeModifier(wm.getModifiers, mod))
      this
    }
  }
  
  implicit def toRichBlock(b: Block) = new RichBlockStmt(b)
  
  class RichBlockStmt(b: Block) {    
    def apply(i: Int) = if (isEmpty) null else b.getStatements.get(i)
    def isEmpty = b.getStatements == null || b.getStatements.isEmpty
    def add(s: Statement) {
      b.setStatements(b.getStatements :+ s)
    }
    def addAll(s: List[Statement]) {
      b.setStatements(b.getStatements ++ s)
    }
    def remove(s: Statement) {
      b.setStatements(b.getStatements.filterNot(_ == s))
    }
    def removeAll(s: List[Statement]) {
      b.setStatements(b.getStatements.filterNot(s.contains))
    }
    def copy(): Block = {
      def block = new Block()
      def stmts = new NodeList[Statement]()
      stmts.addAll(b.getStatements)
      block.setStatements(stmts)
      block
    }
    
    def size = if (b.getStatements != null) b.getStatements.size else 0
  }  
    
  //@inline
  def isEmpty(col: JavaCollection[_]): Boolean = col == null || col.isEmpty

  def nonEmptyOption[A](col: JavaCollection[A]): Option[JavaCollection[A]] =
    if(!isEmpty(col)) Some(col) else None
  
  def getAssignment(s: Statement): Assign = s match {
    case Stmt(a: Assign) => a
    case _ => null
  }
  
  // TODO use pattern matching
  def getLazyInit(block: Block) = {
    block.getStatements.get(0).asInstanceOf[If]
	    .getThenStmt.asInstanceOf[Block]
	    .getStatements.get(0).asInstanceOf[ExpressionStmt]
      .getExpression.asInstanceOf[Assign]
      .getValue
  }
    
  def isLazyCreation(block: Block, f: String): Boolean = block match {
    case Block(
        If(isnull(field(`f`)), Stmt(field(`f`) set init), null) :: 
        Return(field(`f`)) :: Nil) => true
    case _ => false   
  }
        
  def isAssignment(s: Statement): Boolean = s match {
    case Stmt(_ set _) => true
    case _ => false
  }
    
  def isThisConstructor(s: Statement): Boolean = s match {
    case ci: ConstructorInvocation => ci.isThis
    case _ => false
  }
  
  def isStatic(member: BodyDecl): Boolean = member match {
    case t: ClassOrInterfaceDecl => t.isStatic || t.getModifiers.isObject || t.isInterface
    case t: TypeDecl => t.isStatic || t.getModifiers.isObject
    case f: Field => f.isStatic
    case m: Method => m.isStatic
    case i: Initializer => i.isStatic
    case _ => false
  }  
  
  def isHashCode(n: Method): Boolean = n match { 
    case Method("hashCode", Type.Int, Nil, _) => true
    case _ => false
  }
    
  def isEquals(n: Method): Boolean = n match {
    case Method("equals", Type.Boolean,_ :: Nil, _) => true
    case _ => false
  }
    
  def isReturnFieldStmt(stmt: Statement): Boolean = stmt match {
    case Return(field(_)) => true
    case _ => false
  }
  
  def isSetFieldStmt(stmt: Statement): Boolean = stmt match {
    case Stmt(_ set _) => true
    case _ => false
  }
  
  def isToString(n: Method): Boolean = n match {
    case Method("toString", Type.String, Nil, _) => true
    case _ => false
  }

  implicit class RichIterator[T](iter: Iterator[T]) {

    def takeUpToWhere(f: T => Boolean): Iterator[T] = new Iterator[T] {
      var done = false

      def next = {
        if (done) throw new NoSuchElementException()
        val n = iter.next;
        done = f(n)
        n
      }

      def hasNext = {
        !done && iter.hasNext;
      }
    }
  }
}