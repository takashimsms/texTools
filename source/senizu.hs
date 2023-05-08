-- 必要そうなもの
-- \usepackage{tikz}
-- \usetikzlibrary{intersections, calc, arrows, positioning, arrows.meta}

output =
    "\\begin{center}\n\
    \  \\begin{tikzpicture}[node/.style={draw, circle, font=\\Large, inner sep=6pt}]\n"
    
    

menu c s = do
    putStrLn "(n):ノードを作成\n(l):ノード間に線を引く\n(e):終了"
    m <- getLine
    mode m c s
    

    
mode "n" c s = do
    putStrLn "\nmode n\n(n):ノードを新規作成\n(a):他のノードの隣にノードを作成"
    m <- getLine
    modeN m c s

mode "l" c s = modeL c s
mode "e" c s = do
    putStrLn "\n\n実行終了\n\n"
    putStrLn $ s ++ "  \\end{tikzpicture}\n\\end{center}\n\n"

mode _ c s = do
    putStrLn "エラー もう一度モードを選択してください"
    menu c s


-- ノードの新規作成 
modeN "n" c s = do
    let num = show c
    let text = s ++ "    \\node[node] (q" ++ num ++") {$q_{" ++ num ++ "}$};\n"
    putStrLn text
    menu (c + 1) text

modeN "a" c s = do
    let num = show c
    putStrLn "基準にするノードを選んでください"
    base <- getLine
    let endText = "q" ++ base ++ "] (q" ++ num ++ ") {$q_{" ++ num ++ "}$};\n"
    nSelectPos c endText s

modeN _ c s = do 
    putStrLn "エラー もう一度モードを選択してください"
    mode "n" c s



nSelectPos c endText s = do
    putStrLn "配置する場所を選択してください\n\
    \(a):above\n\
    \(b):below\n\
    \(r):right\n\
    \(l):left\n\
    \(ar):above right\n\
    \(al):above left\n\
    \(br):below right\n\
    \(bl):below left"
    pos <- getLine
    case pos of
        "a" -> nSelectDis c "    \\node[node, above = " endText s
        "b" -> nSelectDis c "    \\node[node, below = " endText s
        "r" -> nSelectDis c "    \\node[node, right = " endText s
        "l" -> nSelectDis c "    \\node[node, left = " endText s
        "ar" -> nSelectDis c "    \\node[node, above right = " endText s
        "al" -> nSelectDis c "    \\node[node, above left = " endText s
        "br" -> nSelectDis c "    \\node[node, below right = " endText s
        "bl" -> nSelectDis c "    \\node[node, below left = " endText s
        _ -> do
            putStrLn "エラー もう一度入力してください"
            nSelectPos c endText s



nSelectDis c t endText s = do
    putStrLn "距離は？(半角スペース区切り)"
    x <- getLine
    let y = words x
    case length y of
        1 -> do
            putStrLn $ s ++ t ++ head y ++ "cm of " ++ endText
            menu (c + 1) $ s ++ t ++ head y ++ "cm of " ++ endText
        2 -> do
            putStrLn $ s ++ t ++ head y ++ "cm and " ++ y !! 1 ++ "cm of " ++ endText
            menu (c + 1) $ s ++ t ++ head y ++ "cm and " ++ y !! 1 ++ "cm of " ++ endText
        _ -> do
            putStrLn "エラー もう一度入力してください"
            nSelectDis c t endText s
        


-- mode l
modeL c s = do
    putStrLn "どの線が引きたい？\n(l):ループ\n(b):ノードからノードへ\n"
    m <- getLine
    case m of
        "l" -> lMode c s
        "b" -> bMode c s
        _ -> do
            putStrLn "エラー もう一度入力してください"
            modeL c s



lMode c s = do
    putStrLn "書きたいノードを選択\n例(q0に書きたい場合):0\n"
    node <- getLine
    lSelectDire c s node



lSelectDire c s node = do
    putStrLn "矢印を描画する位置(8方向 a,b,r,l,ar,al,br,bl)"
    dire <- getLine
    case dire of
        "ar" -> lSelectStr c s node $ "    \\path[->, >=stealth] (q" ++ node ++ ") edge[loop above, in=0, out=90, looseness=4] node{"
        "a" -> lSelectStr c s node $ "    \\path[->, >=stealth] (q" ++ node ++ ") edge[loop above, in=45, out=135, looseness=4] node{"
        "al" -> lSelectStr c s node $ "    \\path[->, >=stealth] (q" ++ node ++ ") edge[loop above, in=90, out=180, looseness=4] node{"
        "l" -> lSelectStr c s node $ "    \\path[->, >=stealth] (q" ++ node ++ ") edge[loop above, in=135, out=225, looseness=4] node{"
        "bl" -> lSelectStr c s node $ "    \\path[->, >=stealth] (q" ++ node ++ ") edge[loop above, in=180, out=270, looseness=4] node{"
        "b" -> lSelectStr c s node $ "    \\path[->, >=stealth] (q" ++ node ++ ") edge[loop above, in=225, out=315, looseness=4] node{"
        "br" -> lSelectStr c s node $ "    \\path[->, >=stealth] (q" ++ node ++ ") edge[loop above, in=270, out=0, looseness=4] node{"
        "r" -> lSelectStr c s node $ "    \\path[->, >=stealth] (q" ++ node ++ ") edge[loop above, in=315, out=45, looseness=4] node{"
        _ -> do
            putStrLn "エラー もう一度入力してください"
            lSelectDire c s node



lSelectStr c s node direOut = do
    putStrLn "矢印に書きたい文章を入力"
    text <- getLine
    putStrLn $ s ++ direOut ++ text ++ "} (q" ++ node ++ ");\n"
    menu c $ s ++ direOut ++ text ++ "} (q" ++ node ++ ");\n"



bMode c s = do
    putStrLn "2つのノードを選択(半角スペース区切り)"
    line <- getLine
    let nodes = words line
    if length nodes == 2
        then do
            putStrLn "矢印は双方向？(y):yes\notherwise:no"
            tf <- getLine
            case tf of
                "y" -> bSelectStr c s ("    \\path[<->, >=stealth] (q" ++ head nodes ++ ") edge[above] node{") $ nodes !! 1
                _ -> bSelectAng c s ("    \\path[->, >=stealth] (q" ++ head nodes ++ ") edge[above") $ nodes !! 1
        else do
            putStrLn "エラー もう一度入力してください"
            bMode c s


bSelectStr c s headText endNode = do
    putStrLn "矢印に書きたい文章を入力"
    text <- getLine
    putStrLn $ s ++ headText ++ text ++ "} (q" ++ endNode ++ ");\n"
    menu c $ s ++ headText ++ text ++ "} (q" ++ endNode ++ ");\n"



bSelectAng c s headText endNode = do
    putStrLn "矢印を曲げて描画したい場合は曲げる角度を入力"
    angle <- getLine
    case angle of
        "" -> bSelectStr c s (headText ++"] node{") endNode
        _ -> bSelectStr c s (headText ++ ", bend right=" ++ angle ++ "] node{") endNode



main = do
    putStrLn "状態遷移図作成ツール"
    menu 0 output