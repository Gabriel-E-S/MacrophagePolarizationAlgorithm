async function enviarParaAPI() {
    const input = document.getElementById('inputCsv');
    const status = document.getElementById('mensagemStatus');
    const areaGrafo = document.getElementById('areaGrafo');
    const imgGrafo = document.getElementById('imagemGrafo');

    if (input.isDefaultNamespace.length === 0){
        status.innerText = "Erro, por favor coloque o arquivo .csv antes de clicar em enviar" ;
        status.style.color = "#c0392b"
        return;
    }

    status.innerText = "Enviando dados para processamento... aguarde.";
    status.style.color = "#2980b9"; 
    areaGrafo.style.display = "none";

    const dadosFormulario = new FormData();

    dadosFormulario.append("arquivo_usuario", input.files[0]);

    try {
        // Dispara a requisição para a sua API em R (Back-end)
        const resposta = await fetch("http://127.0.0.1:8000/processar", {
            method: "POST",
            body: dadosFormulario
        });

        if (!resposta.ok) {
            throw new Error("Erro na comunicação com o servidor R.");
        }
        const dados_retornados = await resposta.json();

        fazerDownload(dados_retornados.arquivo1, "RME_resultado_vertices.csv");
        fazerDownload(dados_retornados.arquivo2, "RME_resultado_arestas.csv");

        imgGrafo.src = "data:image/png;base64," + dados_retornados.grafo;
        areaGrafo.style.display = "block";

        status.innerText = "Processamento concluído com sucesso!";
        status.style.color = "#27ae60"; 

    } catch (erro) {
        status.innerText = "Falha ao conectar com o algoritmo. Verifique se o servidor R está rodando.";
        status.style.color = "#c0392b"; 
        console.error(erro);
    }
}

function fazerDownload(conteudoTexto, nomeDoArquivo) {
    
    const blob = new Blob([conteudoTexto], { type: 'text/csv' });
    const link = document.createElement('a');
    link.href = window.URL.createObjectURL(blob);
    link.download = nomeDoArquivo;

    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);


}