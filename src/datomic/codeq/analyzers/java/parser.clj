(ns datomic.codeq.analyzers.java.parser
  (:refer-clojure :exclude [name])
  (:require [datomic.codeq.analyzer :as az]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [org.eclipse.jdt.core.dom AST ASTNode ASTParser
            ArrayType ParameterizedType PrimitiveType SimpleType
            CompilationUnit PackageDeclaration ImportDeclaration
            TypeDeclaration EnumDeclaration AnnotationTypeDeclaration
            MethodDeclaration Statement Expression Type SingleVariableDeclaration]
           [org.eclipse.jdt.core JavaCore]))

(set! *warn-on-reflection* true)

(defn java-parse-tree [^String s]
  (let [parser (ASTParser/newParser AST/JLS3)
        options (JavaCore/getOptions)]
    (.setSource parser (.toCharArray s))
    (.setCompilerOptions parser options)
    (.createAST parser nil)))
  
(defn loc [^ASTNode node]
  (let [spos (.getStartPosition node)
        epos (dec (+ spos (.getLength node)))
        root ^CompilationUnit (.getRoot node)]
    [(.getLineNumber root spos)
     (.getColumnNumber root spos)
     (.getLineNumber root epos)
     (.getColumnNumber root epos)]))

(defprotocol UniqueName
  (name [_]))

(defprotocol ArgName
  (argname [_]))

(extend-protocol ArgName
  ArrayType
  (argname
   [node]
   (str "[" (argname (.getComponentType node))))

  ParameterizedType
  (argname
   [node]
   (str (argname (.getType node))))
  
  SimpleType
  (argname
   [node]
   (str "L" (.getName node) ";"))
  
  PrimitiveType
  (argname
   [node]
   (str (.getPrimitiveTypeCode node)))
  
  SingleVariableDeclaration
  (argname
   [node]
   (argname (.getType node))))

(extend-protocol UniqueName
  nil       ;; see this importing junit, have not tracked down
  (name
   [_] nil)
  
  MethodDeclaration
  (name
   [node]
   (str (-> node .getName .getFullyQualifiedName)
        "("
        (str/join ""
                  (map argname (.parameters node)))
        ")"))
  
  ASTNode
  (name
   [node]
   (-> node .getName .getFullyQualifiedName)))

(defmulti parse* type)

(defn parse [node]
  (let [src (str node)]
    (merge {:loc (loc node)
            :src src
            :sha (az/sha src)}
           (parse* node))))

(defmethod parse* CompilationUnit [^CompilationUnit node]
  {:node :compilation-unit
   :package (name (.getPackage node))
   :imports (map name (.imports node))
   :types (map parse (.types node))})

(defmethod parse* TypeDeclaration [^TypeDeclaration node]
  (merge {:node :type-declaration
          :name (name node)
          :fields (map parse (.getFields node))
          :methods (map parse (.getMethods node))
          :types (map parse (.getTypes node))
          :interface? (.isInterface node)}
         (when-let [super (.getSuperclassType node)]
           {:superclass (parse super)})))

(defmethod parse* EnumDeclaration [node]
  {:node :enum-declaration
   :name (name node)})

(defmethod parse* AnnotationTypeDeclaration [node]
  {:node :annotation-type-declaration
   :name (name node)})

(defmethod parse* MethodDeclaration [^MethodDeclaration node]
  (merge
   {:node :method-declaration
    :name (name node)
    :parameters (map parse (.parameters node))
    :constructor? (.isConstructor node)
    :varargs? (.isVarargs node)}
   (when-let [body (.getBody node)]
     {:body (parse body)})
   (when-let [ret-type (.getReturnType2 node)]
     {:return-type (parse ret-type)})))


(defmethod parse* SingleVariableDeclaration [node]
  {:node :variable-declaration
   :name (name node)})

(defmethod parse* Statement [node]
  {:node :statement})

(defmethod parse* Expression [node]
  {:node :expression})

(defmethod parse* Type [node]
  {:node :type})

(defn parse-tree [s]
  (-> s java-parse-tree parse))
  
;;; Eclipse JDT API inheritance tree
'{ASTNode
  [{Expression [{Annotation [MarkerAnnotation NormalAnnotation SingleMemberAnnotation]
                 Name [QualifiedName SimpleName]}
                Assignment ArrayInitializer ArrayAccess BooleanLiteral CastExpression CharacterLiteral ClassInstanceCreation ConditionalExpression FieldAccess
                InfixExpression InstanceOfExpression MethodInvocation NullLiteral NumberLiteral ParenthesizedExpression PostfixExpression PrefixExpression
                StringLiteral SuperFieldAccess SuperMethodInvocation ThisExpression VariableDeclarationExpression TypeLiteral]
    Statement [Block AssertStatement BreakStatement ConstructorInvocation ContinueStatement DoStatement EmptyStatement EnhancedForStatement ExpressionStatement
               ForStatement IfStatement LabeledStatement ReturnStatement SuperConstructorInvocation SwitchCase SwitchStatement SynchronizedStatement ThrowStatement
               TryStatement TypeDeclarationStatement VariableDeclarationStatement WhileStatement]
    Type [ArrayType ParameterizedType PrimitiveType QualifiedType SimpleType UnionType WildcardType]
    Comment [BlockComment JavaDoc LineComment]
    BodyDeclaration [{AbstractTypeDeclaration [AnnotationTypeDeclaration EnumDeclaration TypeDeclaration]}
                     AnnotationTypeMemberDeclaration EnumConstantDeclaration FieldDeclaration MethodDeclaration Initializer]
    VariableDeclaration [SingleVariableDeclaration VariableDeclarationFragment]}
   PackageDeclaration AnonymousClassDeclaration CatchClause CompilationUnit ImportDeclaration MemberRef MemberValuePair MethodRef MethodRefParameter PackageDeclaration
   TagElement TextElement TypeParameter]}
