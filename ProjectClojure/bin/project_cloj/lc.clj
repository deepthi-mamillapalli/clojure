(ns lc.core(:require [instaparse.core :as insta])
    (:import(clojure.asm Opcodes Type ClassWriter)
          (clojure.asm.commons Method GeneratorAdapter)))


(defn dynamic-eval [interpreter]
 (fn[ast]
 (fn[]
 (insta/transform interpreter ast))))

(def parser-arith
  (insta/parser
   "<prog> =  addop ?spaces addop ?spaces
    <addop> = mulop | add 
    add = addop spaces <'+'> spaces mulop
    <mulop> = term | mul 
    mul = mulop spaces <'*'> spaces term
    expo = term spaces? <'^'> spaces? number
    <term> = number | <'('>  addop  <')'> | expo
   
    <spaces> = <#'[ ]'*>
    number = #'[0-9]+'"))

(parser-arith "50445567878748788774878 * 9548445114781512151870 + 5645515419532121 ^ 54   ")


;Interpreter
(defn make-interpreting [make-instr-interpreting init-env]
 {:prog (fn [& instrs] (:_ret (reduce
 (fn[env instr]
 (insta/transform (make-instr-interpreting env) instr))
 init-env
 instrs)))})

   (defn arith-instr-interpreting [env]
 { 
 :add (fn[{v1 :_ret :as env1} {v2 :_ret :as env2}]
 (assoc (merge env1 env2) :_ret (+ v1 v2)))
 
 :mult (fn[{v1 :_ret :as env1} {v2 :_ret :as env2}]
 (assoc (merge env1 env2) :_ret (* v1 v2)))
 
 :number #(assoc env :_ret (Long/parseLong %))
 
 
 })
 

(def arith-interpret (dynamic-eval (make-interpreting arith-instr-interpreting {:_ret 0})))
 
(def arith-interpret-test (->> "(999934344542+234) * 4 ^ 6 " parser-arith arith-interpret))
 (arith-interpret-test)

;Compiler
 (defn generate-instr [mv instr]
   "Generate the method call to an  org.objectweb.asm.MethodVisitor for a given instruction."
   (do (print instr)
   (condp = (first instr)
     :load (.visitVarInsn mv Opcodes/ILOAD (int (second instr)))
     :store (.visitVarInsn mv Opcodes/ISTORE (int (second instr)))
     :loadi (.visitLdcInsn mv (int (second instr)))
     :addi (.visitInsn mv Opcodes/IADD)
     :multi (.visitInsn mv Opcodes/IMUL)
     :reti (.visitInsn mv Opcodes/IRETURN)
     ))
   mv)

(defn compile-ast [name ast]
"Generate a class of given name with a run method implementing the given ast."
  (let [op-fun (fn[op]
                 (fn[instrs-v0 instrs-v1]
                   (conj (into instrs-v0 instrs-v1) [op])))
        prog (insta/transform {:prog (fn[ & instrs] (conj (reduce into [[:loadi 0]] instrs) [:reti]))
                               :add (op-fun :addi)
                               :mul (op-fun :multi)
                               :number #(vector [:loadi (Long/parseLong %)])}
                              ast)
        generate-prog #(reduce generate-instr % prog)
        cw (ClassWriter. (+ ClassWriter/COMPUTE_FRAMES ClassWriter/COMPUTE_MAXS ))
        init (Method/getMethod "void <init>()")
        meth-name "run"
        meth-sig "()I"]
      (.visit cw Opcodes/V1_6 Opcodes/ACC_PUBLIC (.replace name \. \/) nil "java/lang/Object" nil)
      (doto (GeneratorAdapter. Opcodes/ACC_PUBLIC init nil nil cw)
        (.visitCode)
        (.loadThis)
        (.invokeConstructor (Type/getType Object) init)
        (.returnValue)
        (.endMethod))
      (doto (.visitMethod cw (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) meth-name meth-sig nil nil )
        (.visitCode)
        (generate-prog)
        (.visitMaxs 0 0 )
        (.visitEnd))
      (.visitEnd cw)
      (let [b (.toByteArray cw)
            cl (clojure.lang.DynamicClassLoader.)]
        (.defineClass cl name b nil))
      (fn [] (clojure.lang.Reflector/invokeStaticMethod name meth-name (into-array [])))))

(def compiled (compile-ast "test" (conj [:prog ] (first (parser-arith "(2524354 + 77771) * (7773 + 8884)")))))
(compiled)