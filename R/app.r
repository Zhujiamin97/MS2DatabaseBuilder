# 1. 安装必要包（如果尚未安装）
# 请在R控制台运行以下命令：
# install.packages(c("shiny", "shinydashboard", "DT", "plotly", "shinyjs", "mzR"))

# 2. 加载所需库
library(shiny)
library(DT)
library(plotly)
library(shinyjs)
library(mzR)
library(dplyr)
library(tidyr)
library(readr)


# ==================== UI部分 ====================
ui <- fluidPage(
  # 自定义CSS样式
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Arial, sans-serif;
        font-size: 13px;
        padding: 10px;
        background-color: #f5f7fa;
      }
      .main-header {
        text-align: center;
        margin-bottom: 20px;
        padding-bottom: 15px;
        border-bottom: 2px solid #2c3e50;
      }
      .upload-section {
        background-color: white;
        padding: 20px;
        border-radius: 8px;
        border: 1px solid #dee2e6;
        margin-bottom: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .result-section {
        background-color: white;
        padding: 20px;
        border-radius: 8px;
        border: 1px solid #dee2e6;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .btn-success {
        background: linear-gradient(to right, #28a745, #20c997);
        border: none;
        width: 100%;
        padding: 12px;
        font-size: 14px;
        font-weight: bold;
        border-radius: 5px;
        transition: all 0.3s;
      }
      .btn-success:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(40, 167, 69, 0.3);
      }
      .btn-primary {
        background: linear-gradient(to right, #007bff, #0056b3);
        border: none;
        border-radius: 5px;
        padding: 8px 15px;
        transition: all 0.3s;
      }
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0, 123, 255, 0.3);
      }
      .progress-container {
        position: fixed;
        bottom: 20px;
        right: 20px;
        width: 320px;
        background-color: white;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        z-index: 1000;
      }
      .progress-label {
        font-size: 13px;
        margin-bottom: 8px;
        color: #2c3e50;
      }
      .plot-container {
        height: 400px;
        margin-bottom: 20px;
        border: 1px solid #eee;
        border-radius: 5px;
        padding: 10px;
        background-color: white;
      }
      h4 {
        color: #2c3e50;
        margin-bottom: 15px;
        padding-bottom: 10px;
        border-bottom: 2px solid #3498db;
        font-weight: 600;
      }
      .file-info {
        background-color: #e8f4fd;
        padding: 10px;
        border-radius: 5px;
        border-left: 4px solid #3498db;
        margin-top: 10px;
        font-size: 12px;
        color: #2c3e50;
      }
      .modal-dialog {
        max-width: 95%;
        width: 95%;
      }
      .modal-content {
        border-radius: 10px;
        border: none;
        box-shadow: 0 10px 30px rgba(0,0,0,0.2);
      }
      .modal-header {
        background: linear-gradient(to right, #2c3e50, #4a6491);
        color: white;
        border-radius: 10px 10px 0 0;
        border-bottom: none;
        padding: 15px 20px;
      }
      .modal-header .close {
        color: white;
        text-shadow: none;
        opacity: 0.8;
        font-size: 24px;
      }
      .modal-header .close:hover {
        opacity: 1;
      }
      .modal-body {
        padding: 20px;
        background-color: #f8f9fa;
      }
      .parameter-box {
        background-color = #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        border: 1px solid #dee2e6;
        margin-bottom: 10px;
      }
      .nav-tabs {
        border-bottom: 2px solid #dee2e6;
        margin-bottom: 15px;
      }
      .nav-tabs .nav-link.active {
        background-color: #007bff;
        color: white;
        border-color: #007bff;
        font-weight: bold;
      }
    "))
  ),
  
  useShinyjs(),
  
  # 主标题
  div(
    class = "main-header",
    h3("Build ms2 database v1.0", style = "color: #2c3e50; font-weight: 700;"),
    #h5("版本 1.1", style = "color: #7f8c8d; font-style: italic;")
  ),
  
  # 上传和处理区域
  div(
    class = "upload-section",
    fluidRow(
      column(
        width = 5,
        h4("📁 文件上传"),
        
        fileInput(
          "compound_file",
          "上传化合物CSV文件",
          accept = c(".csv", ".txt"),
          buttonLabel = "浏览...",
          placeholder = "选择CSV文件",
          width = "100%"
        ),
        
        fileInput(
          "ms2_file",
          "上传质谱数据文件",
          accept = c(".mzML", ".mzXML"),
          buttonLabel = "浏览...",
          placeholder = "选择mzML/mzXML文件",
          width = "100%"
        ),
        
        uiOutput("file_info_ui"),
        
        hr(),
        
        h4("⚙️ 处理控制"),
        
        actionButton("start_processing", "开始构建数据库", 
                     icon = icon("database"), 
                     class = "btn-success",
                     style = "margin-top: 10px;")
      ),
      
      column(
        width = 7,
        h4("🔧 参数设置"),
        
        tabsetPanel(
          tabPanel(
            "基本参数",
            div(
              class = "parameter-box",
              fluidRow(
                column(6,
                       numericInput("mz_tol", "m/z容差 (Da)", 
                                    value = 0.01, min = 0.001, max = 0.1, step = 0.001,
                                    width = "100%")
                ),
                column(6,
                       numericInput("ppm", "ppm容差", 
                                    value = 20, min = 5, max = 100, step = 5,
                                    width = "100%")
                )
              ),
              
              fluidRow(
                column(6,
                       numericInput("rt_tol", "RT容差 (秒)", 
                                    value = 30, min = 5, max = 300, step = 5,
                                    width = "100%")
                ),
                column(6,
                       numericInput("min_intensity", "最小强度", 
                                    value = 100, min = 1, max = 1000, step = 10,
                                    width = "100%")
                )
              )
            )
          ),
          
          tabPanel(
            "EIC参数",
            div(
              class = "parameter-box",
              fluidRow(
                column(6,
                       numericInput("eic_mz_tol", "EIC m/z容差", 
                                    value = 0.01, min = 0.001, max = 0.1, step = 0.001,
                                    width = "100%")
                ),
                column(6,
                       numericInput("eic_rt_window", "EIC RT窗口 (秒)", 
                                    value = 30, min = 5, max = 300, step = 5,
                                    width = "100%")
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # 结果展示区域
  div(
    class = "result-section",
    h4("📊 数据库结果"),
    
    fluidRow(
      column(
        width = 12,
        div(
          style = "margin-bottom: 15px; text-align: center;",
          downloadButton("download_csv", "下载CSV", class = "btn-primary", 
                         style = "margin-right: 5px;"),
          downloadButton("download_rds", "下载RDS", class = "btn-primary", 
                         style = "margin-right: 5px;"),
          downloadButton("download_msp", "下载MSP", class = "btn-primary")
        ),
        
        DTOutput("database_table")
      )
    ),
    
    hr()
    
    # h4("📈 数据库统计"),
    # 
    # fluidRow(
    #   column(4, plotlyOutput("match_quality_plot", height = "250px")),
    #   column(4, plotlyOutput("mz_error_plot", height = "250px")),
    #   column(4, plotlyOutput("rt_error_plot", height = "250px"))
    # )
  ),
  
  # 右下角进度条
  uiOutput("progress_ui"),
  
  # 可视化弹窗 - 使用bootstrap modal
  tags$div(
    id = "visualizationModal",
    class = "modal fade",
    tabindex = "-1",
    role = "dialog",
    `aria-labelledby` = "visualizationModalLabel",
    `aria-hidden` = "true",
    tags$div(
      class = "modal-dialog modal-xl",
      role = "document",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h4(class = "modal-title", id = "visualizationModalLabel", 
                  textOutput("modal_title")),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            onclick = "$('#visualizationModal').modal('hide');",
            tags$span(`aria-hidden` = "true", "×")
          )
        ),
        tags$div(
          class = "modal-body",
          fluidRow(
            column(
              width = 12,
              div(
                class = "plot-container",
                plotlyOutput("modal_eic_plot", height = "350px")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              div(
                class = "plot-container",
                plotlyOutput("modal_ms2_plot", height = "350px")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              style = "text-align: center; margin-top: 20px;",
              downloadButton("download_combined", "下载谱图数据", 
                             class = "btn-primary",
                             style = "padding: 10px 20px; font-size: 14px;")
            )
          )
        )
      )
    )
  ),
  
  # 初始化模态框的JavaScript
  tags$script(HTML("
    // 初始化模态框
    $(document).ready(function() {
      $('#visualizationModal').modal({show: false, backdrop: 'static'});
      
      // 监听关闭按钮点击
      $('#visualizationModal .close').click(function() {
        $('#visualizationModal').modal('hide');
      });
      
      // 监听模态框外部点击关闭
      $('#visualizationModal').on('click', function(e) {
        if (e.target === this) {
          $('#visualizationModal').modal('hide');
        }
      });
    });
    
    // 显示模态框的函数
    function showVisualizationModal() {
      $('#visualizationModal').modal('show');
    }
  "))
)

# ==================== 服务器部分 ====================
server <- function(input, output, session) {
  
  # ==================== 响应式值 ====================
  values <- reactiveValues(
    compounds = NULL,
    database = NULL,
    processing = FALSE,
    progress_value = 0,
    progress_text = "等待开始",
    selected_spec = NULL
  )
  
  # ==================== 文件上传自动处理 ====================
  # 显示文件信息
  output$file_info_ui <- renderUI({
    if (!is.null(input$compound_file) && !is.null(input$ms2_file)) {
      div(
        class = "file-info",
        tags$small(
          icon("check-circle", style = "color: #28a745; margin-right: 5px;"),
          tags$b("文件已上传:"),
          br(),
          paste("化合物文件:", tags$code(input$compound_file$name)),
          br(),
          paste("质谱文件:", tags$code(input$ms2_file$name))
        )
      )
    }
  })
  
  # 自动读取化合物数据
  observeEvent(input$compound_file, {
    req(input$compound_file)
    
    tryCatch({
      compounds <- read.csv(input$compound_file$datapath, stringsAsFactors = FALSE)
      
      # 验证必要的列
      required_cols <- c("name", "mz", "rt")
      missing_cols <- setdiff(required_cols, colnames(compounds))
      
      if (length(missing_cols) > 0) {
        showNotification(
          paste("CSV文件缺少必要的列:", paste(missing_cols, collapse = ", ")),
          type = "error",
          duration = 5
        )
        return()
      }
      
      # 检查RT单位，如果小于60，可能为分钟，转换为秒
      if(max(compounds$rt, na.rm = TRUE) < 60) {
        compounds$rt <- compounds$rt * 60
        showNotification("检测到保留时间单位为分钟，已自动转换为秒", 
                        type = "warning", duration = 3)
      }
      
      values$compounds <- compounds
      
      showNotification(
        paste("成功加载", nrow(compounds), "个化合物"),
        type = "message", duration = 3
      )
      
    }, error = function(e) {
      showNotification(paste("读取文件时出错:", e$message), 
                      type = "error", duration = 5)
    })
  })
  
  # ==================== 进度条UI ====================
  output$progress_ui <- renderUI({
    if (values$processing) {
      div(
        class = "progress-container",
        div(
          class = "progress-label",
          icon("cog", class = "fa-spin", style = "margin-right: 8px;"),
          tags$b(values$progress_text)
        ),
        div(
          class = "progress",
          style = "height: 20px; border-radius: 10px; overflow: hidden;",
          div(
            class = "progress-bar progress-bar-striped progress-bar-animated",
            role = "progressbar",
            style = paste0("width: ", values$progress_value, "%; background-color: #3498db;"),
            paste0(values$progress_value, "%")
          )
        )
      )
    }
  })
  
  # 更新进度函数
  update_progress <- function(value, text = NULL) {
    values$progress_value <- value
    if (!is.null(text)) {
      values$progress_text <- text
    }
  }
  
  # ==================== 数据库构建 ====================
  observeEvent(input$start_processing, {
    req(input$compound_file, input$ms2_file, values$compounds)
    
    # 重置进度
    update_progress(0, "开始构建数据库...")
    values$processing <- TRUE
    
    # 禁用开始按钮，防止重复点击
    shinyjs::disable("start_processing")
    
    # 步骤1: 读取质谱数据文件
    showNotification("步骤 1/4: 正在读取质谱数据文件...", 
                    type = "default", 
                    duration = NULL, 
                    id = "step_notification")
    update_progress(10, "读取质谱数据文件...")
    
    tryCatch({
      ms2_file_path <- input$ms2_file$datapath
      if (!file.exists(ms2_file_path)) {
        stop("MS2数据文件不存在")
      }
      
      msdata <- mzR::openMSfile(ms2_file_path)
      header_info <- mzR::header(msdata)
      
      # 步骤2: 提取MS2扫描信息
      removeNotification(id = "step_notification")
      showNotification("步骤 2/4: 正在提取MS2扫描信息...", 
                      type = "default", 
                      duration = NULL, 
                      id = "step_notification")
      update_progress(30, "提取MS2扫描信息...")
      
      ms2_scans <- header_info[header_info$msLevel == 2, ]
      
      if (nrow(ms2_scans) == 0) {
        mzR::close(msdata)
        stop("文件中没有MS2扫描数据")
      }
      
      # 提取MS2谱图
      ms2_spectra <- list()
      for (i in 1:nrow(ms2_scans)) {
        scan_num <- ms2_scans$seqNum[i]
        spectrum <- mzR::peaks(msdata, scan_num)
        
        if (nrow(spectrum) > 0) {
          spectrum <- spectrum[spectrum[, 2] >= input$min_intensity, , drop = FALSE]
          
          if (nrow(spectrum) >= 5) {
            top_n_peaks <- 50
            if (nrow(spectrum) > top_n_peaks) {
              spectrum <- spectrum[order(spectrum[, 2], decreasing = TRUE)[1:top_n_peaks], ]
              spectrum <- spectrum[order(spectrum[, 1]), ]
            }
            
            ms2_spectra[[i]] <- list(
              scan_num = scan_num,
              precursor_mz = ms2_scans$precursorMZ[i],
              precursor_intensity = ms2_scans$precursorIntensity[i],
              retention_time = ms2_scans$retentionTime[i],
              spectrum = spectrum
            )
          }
        }
      }
      
      ms2_spectra <- ms2_spectra[!sapply(ms2_spectra, is.null)]
      
      # 步骤3: 关联化合物与MS2谱图
      removeNotification(id = "step_notification")
      showNotification("步骤 3/4: 正在关联化合物与MS2谱图...", 
                      type = "default", 
                      duration = NULL, 
                      id = "step_notification")
      update_progress(50, "关联化合物与MS2谱图...")
      
      match_results <- data.frame(
        compound_id = integer(),
        compound_name = character(),
        compound_mz = numeric(),
        compound_rt = numeric(),
        scan_num = integer(),
        precursor_mz = numeric(),
        retention_time = numeric(),
        precursor_intensity = numeric(),
        match_quality = character(),
        n_peaks = integer(),
        stringsAsFactors = FALSE
      )
      
      compound_spectra <- list()
      total_compounds <- nrow(values$compounds)
      
      for (i in 1:total_compounds) {
        compound <- values$compounds[i, ]
        
        # 更新进度
        progress_value <- 50 + (i / total_compounds) * 30
        update_progress(progress_value, 
                       sprintf("处理化合物 %d/%d: %s", i, total_compounds, compound$name))
        
        mz_tolerance <- max(input$mz_tol, compound$mz * input$ppm / 1e6)
        candidate_spectra <- list()
        
        for (spec in ms2_spectra) {
          mz_diff <- abs(spec$precursor_mz - compound$mz)
          rt_diff <- abs(spec$retention_time - compound$rt)
          
          if (mz_diff <= mz_tolerance && rt_diff <= input$rt_tol) {
            mz_score <- 1 - (mz_diff / mz_tolerance)
            rt_score <- 1 - (rt_diff / input$rt_tol)
            intensity_score <- log10(spec$precursor_intensity + 1) / 10
            
            total_score <- mz_score * 0.5 + rt_score * 0.3 + intensity_score * 0.2
            
            candidate_spectra[[length(candidate_spectra) + 1]] <- list(
              spec_data = spec,
              mz_diff = mz_diff,
              rt_diff = rt_diff,
              score = total_score
            )
          }
        }
        
        if (length(candidate_spectra) > 0) {
          scores <- sapply(candidate_spectra, function(x) x$score)
          best_idx <- which.max(scores)
          best_match <- candidate_spectra[[best_idx]]
          spec <- best_match$spec_data
          
          if (best_match$score > 0.8) {
            quality <- "excellent"
          } else if (best_match$score > 0.6) {
            quality <- "good"
          } else if (best_match$score > 0.4) {
            quality <- "fair"
          } else {
            quality <- "poor"
          }
          
          match_results <- rbind(match_results, data.frame(
            compound_id = i,
            compound_name = compound$name,
            compound_mz = compound$mz,
            compound_rt = compound$rt,
            scan_num = spec$scan_num,
            precursor_mz = spec$precursor_mz,
            retention_time = spec$retention_time,
            precursor_intensity = spec$precursor_intensity,
            match_quality = quality,
            n_peaks = nrow(spec$spectrum),
            stringsAsFactors = FALSE
          ))
          
          # 提取EIC数据
          eic_data <- extract_eic_for_compound(
            msdata = msdata,
            compound_mz = compound$mz,
            compound_rt = spec$retention_time,
            mz_tolerance = input$eic_mz_tol,
            rt_window = input$eic_rt_window
          )
          
          compound_spectra[[i]] <- list(
            compound_name = compound$name,
            compound_mz = compound$mz,
            compound_rt = compound$rt,
            spectrum = spec$spectrum,
            scan_num = spec$scan_num,
            precursor_mz = spec$precursor_mz,
            precursor_intensity = spec$precursor_intensity,
            retention_time = spec$retention_time,
            eic_data = eic_data,
            match_score = best_match$score,
            match_quality = quality
          )
        } else {
          match_results <- rbind(match_results, data.frame(
            compound_id = i,
            compound_name = compound$name,
            compound_mz = compound$mz,
            compound_rt = compound$rt,
            scan_num = NA,
            precursor_mz = NA,
            retention_time = NA,
            precursor_intensity = NA,
            match_quality = "no_match",
            n_peaks = 0,
            stringsAsFactors = FALSE
          ))
          
          compound_spectra[[i]] <- list(
            compound_name = compound$name,
            compound_mz = compound$mz,
            compound_rt = compound$rt,
            spectrum = NULL,
            eic_data = NULL,
            match_quality = "no_match"
          )
        }
      }
      
      # 步骤4: 构建数据库对象
      removeNotification(id = "step_notification")
      showNotification("步骤 4/4: 正在构建数据库...", 
                      type = "default", 
                      duration = NULL, 
                      id = "step_notification")
      update_progress(90, "构建数据库...")
      
      database <- list(
        metadata = list(
          creation_date = Sys.time(),
          input_files = list(
            compound_csv = input$compound_file$name,
            ms2_file = input$ms2_file$name
          ),
          parameters = list(
            mz_tol = input$mz_tol,
            ppm = input$ppm,
            rt_tol = input$rt_tol,
            min_intensity = input$min_intensity,
            eic_mz_tol = input$eic_mz_tol,
            eic_rt_window = input$eic_rt_window
          ),
          summary = list(
            n_compounds = total_compounds,
            n_matched = sum(match_results$match_quality != "no_match"),
            match_quality = table(match_results$match_quality)
          )
        ),
        compounds = values$compounds,
        match_results = match_results,
        spectra = compound_spectra
      )
      
      values$database <- database
      
      # 关闭质谱文件
      mzR::close(msdata)
      
      update_progress(100, "数据库构建完成！")
      
      # 延迟后完成处理
      shinyjs::delay(1000, {
        removeNotification(id = "step_notification")
        showNotification(
          paste("✅ 数据库构建完成！共匹配", 
                sum(match_results$match_quality != "no_match"), 
                "个化合物"),
          type = "message", duration = 5
        )
        
        values$processing <- FALSE
        shinyjs::enable("start_processing")
      })
      
    }, error = function(e) {
      update_progress(0, paste("错误:", e$message))
      removeNotification(id = "step_notification")
      showNotification(
        paste("❌ 构建数据库时出错:", e$message),
        type = "error", duration = 10
      )
      values$processing <- FALSE
      shinyjs::enable("start_processing")
    })
  })
  
  # ==================== 数据库表格 ====================
  output$database_table <- renderDT({
    req(values$database)
    
    display_data <- values$database$match_results
    
    # 创建操作按钮列
    display_data$action <- sapply(1:nrow(display_data), function(i) {
      if (display_data$match_quality[i] != "no_match") {
        as.character(
          actionButton(
            paste0("view_", i),
            label = "查看谱图",
            icon = icon("chart-line"),
            class = "btn-sm btn-primary",
            onclick = sprintf("Shiny.setInputValue('view_row', %d);", i)
          )
        )
      } else {
        "无匹配"
      }
    })
    
    datatable(
      display_data,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "450px",
        scroller = TRUE,
        searching = TRUE,
        dom = 'Bfrtip',
        buttons = c('pageLength', 'colvis'),
        columnDefs = list(
          list(
            targets = 7,  # precursor_intensity列
            render = JS(
              "function(data, type, row) {
                if (type === 'display') {
                  if (data === null || data === 'NA' || data === '') return 'NA';
                  var num = parseFloat(data);
                  if (num >= 10000) {
                    return num.toExponential(3);
                  } else {
                    return num.toLocaleString('en-US', {minimumFractionDigits: 0, maximumFractionDigits: 0});
                  }
                }
                return data;
              }"
            )
          ),
          list(
            targets = 8,  # match_quality列
            render = JS(
              "function(data, type, row, meta) {
                if (data === 'excellent') {
                  return '<span class=\"badge\" style=\"background-color: #28a745; color: white; padding: 4px 8px; border-radius: 12px;\">' + data + '</span>';
                } else if (data === 'good') {
                  return '<span class=\"badge\" style=\"background-color: #007bff; color: white; padding: 4px 8px; border-radius: 12px;\">' + data + '</span>';
                } else if (data === 'fair') {
                  return '<span class=\"badge\" style=\"background-color: #ffc107; color: white; padding: 4px 8px; border-radius: 12px;\">' + data + '</span>';
                } else if (data === 'poor') {
                  return '<span class=\"badge\" style=\"background-color: #dc3545; color: white; padding: 4px 8px; border-radius: 12px;\">' + data + '</span>';
                } else {
                  return '<span class=\"badge\" style=\"background-color: #6c757d; color: white; padding: 4px 8px; border-radius: 12px;\">' + data + '</span>';
                }
              }"
            )
          ),
          list(
            targets = 10,  # action列
            orderable = FALSE,
            searchable = FALSE
          )
        )
      ),
      class = 'cell-border stripe hover compact',
      selection = 'none',
      rownames = FALSE,
      escape = FALSE
    ) %>% 
      formatRound(columns = c('compound_mz', 'precursor_mz'), digits = 4) %>%
      formatRound(columns = c('compound_rt', 'retention_time'), digits = 2)
  })
  
  # ==================== 数据库统计图表 ====================
  # output$match_quality_plot <- renderPlotly({
  #   req(values$database)
  #   
  #   match_stats <- as.data.frame(table(values$database$match_results$match_quality))
  #   colnames(match_stats) <- c("Quality", "Count")
  #   
  #   colors <- c(
  #     "excellent" = "#28a745",
  #     "good" = "#007bff",
  #     "fair" = "#ffc107",
  #     "poor" = "#dc3545",
  #     "no_match" = "#6c757d"
  #   )
  #   
  #   plot_ly(
  #     match_stats,
  #     x = ~Quality,
  #     y = ~Count,
  #     type = 'bar',
  #     marker = list(color = ~Quality, colors = colors),
  #     text = ~Count,
  #     textposition = 'auto',
  #     hovertemplate = paste(
  #       "<b>%{x}</b><br>",
  #       "数量: %{y}<br>",
  #       "<extra></extra>"
  #     )
  #   ) %>%
  #     layout(
  #       title = list(text = "匹配质量统计", font = list(size = 14)),
  #       xaxis = list(title = "匹配质量", tickfont = list(size = 11)),
  #       yaxis = list(title = "化合物数量", tickfont = list(size = 11)),
  #       showlegend = FALSE,
  #       margin = list(l = 60, r = 30, t = 40, b = 60),
  #       plot_bgcolor = 'rgba(240, 240, 240, 0.5)'
  #     )
  # })
  # 
  # output$mz_error_plot <- renderPlotly({
  #   req(values$database)
  #   
  #   matched_data <- values$database$match_results
  #   matched_data <- matched_data[matched_data$match_quality != "no_match", ]
  #   
  #   if (nrow(matched_data) > 0) {
  #     matched_data$mz_error_ppm <- (matched_data$precursor_mz - matched_data$compound_mz) / 
  #       matched_data$compound_mz * 1e6
  #     
  #     plot_ly(
  #       matched_data,
  #       x = ~mz_error_ppm,
  #       type = 'histogram',
  #       nbinsx = 20,
  #       marker = list(
  #         color = '#007bff',
  #         line = list(color = 'white', width = 1)
  #       ),
  #       hovertemplate = paste(
  #         "<b>m/z误差范围</b><br>",
  #         "误差: %{x:.2f} ppm<br>",
  #         "数量: %{y}",
  #         "<extra></extra>"
  #       )
  #     ) %>%
  #       layout(
  #         title = list(text = "m/z匹配误差分布", font = list(size = 14)),
  #         xaxis = list(title = "m/z误差 (ppm)", tickfont = list(size = 11)),
  #         yaxis = list(title = "频数", tickfont = list(size = 11)),
  #         margin = list(l = 60, r = 30, t = 40, b = 60),
  #         plot_bgcolor = 'rgba(240, 240, 240, 0.5)'
  #       )
  #   }
  # })
  # 
  # output$rt_error_plot <- renderPlotly({
  #   req(values$database)
  #   
  #   matched_data <- values$database$match_results
  #   matched_data <- matched_data[matched_data$match_quality != "no_match", ]
  #   
  #   if (nrow(matched_data) > 0) {
  #     matched_data$rt_error <- matched_data$retention_time - matched_data$compound_rt
  #     
  #     plot_ly(
  #       matched_data,
  #       x = ~rt_error,
  #       type = 'histogram',
  #       nbinsx = 20,
  #       marker = list(
  #         color = '#28a745',
  #         line = list(color = 'white', width = 1)
  #       ),
  #       hovertemplate = paste(
  #         "<b>RT误差范围</b><br>",
  #         "误差: %{x:.2f} 秒<br>",
  #         "数量: %{y}",
  #         "<extra></extra>"
  #       )
  #     ) %>%
  #       layout(
  #         title = list(text = "RT匹配误差分布", font = list(size = 14)),
  #         xaxis = list(title = "RT误差 (秒)", tickfont = list(size = 11)),
  #         yaxis = list(title = "频数", tickfont = list(size = 11)),
  #         margin = list(l = 60, r = 30, t = 40, b = 60),
  #         plot_bgcolor = 'rgba(240, 240, 240, 0.5)'
  #       )
  #   }
  # })
  
  # ==================== 弹窗可视化 ====================
  # 监听查看按钮点击
  observeEvent(input$view_row, {
    req(values$database, input$view_row)
    
    row_index <- input$view_row
    if (row_index <= nrow(values$database$match_results)) {
      selected_compound <- values$database$match_results[row_index, ]
      spec <- values$database$spectra[[selected_compound$compound_id]]
      
      if (!is.null(spec)) {
        values$selected_spec <- spec
        
        # 更新弹窗标题
        output$modal_title <- renderText({
          paste(selected_compound$compound_name, "- MS2谱图可视化")
        })
        
        # 更新EIC图 - 修复：纵坐标从0开始，使用线性坐标
        output$modal_eic_plot <- renderPlotly({
          if (!is.null(spec$eic_data) && nrow(spec$eic_data) > 0) {
            eic_data <- spec$eic_data
            eic_ms1 <- eic_data[eic_data$msLevel == 1, ]
            eic_ms2 <- eic_data[eic_data$msLevel == 2, ]
            
            # 确保EIC数据正确排序
            eic_ms1 <- eic_ms1[order(eic_ms1$retentionTime), ]
            eic_ms2 <- eic_ms2[order(eic_ms2$retentionTime), ]
            
            # 计算合适的y轴范围
            y_max <- if (nrow(eic_ms1) > 0) {
              max(eic_ms1$EIC, na.rm = TRUE)
            } else {
              0
            }
            
            # 如果有MS2点，考虑它们的高度
            if (nrow(eic_ms2) > 0) {
              y_max <- max(y_max, max(eic_ms2$EIC, na.rm = TRUE))
            }
            
            # 添加一些余量
            if (y_max > 0) {
              y_max <- y_max * 1.1
            } else {
              y_max <- 1  # 避免y_max为0
            }
            
            plot_ly() %>%
              add_trace(
                data = eic_ms1,
                x = ~retentionTime,
                y = ~EIC,
                type = 'scatter',
                mode = 'lines',
                name = 'MS1 EIC',
                line = list(color = 'rgb(65, 105, 225)', width = 2.5),
                fill = 'tozeroy',
                fillcolor = 'rgba(65, 105, 225, 0.1)',
                hovertemplate = paste(
                  "<b>MS1扫描</b><br>",
                  "RT: %{x:.2f} s<br>",
                  "强度: %{y:.2e}",
                  "<extra></extra>"
                )
              ) %>%
              add_trace(
                data = eic_ms2,
                x = ~retentionTime,
                y = ~EIC,
                type = 'scatter',
                mode = 'markers',
                name = 'MS2触发点',
                marker = list(
                  color = 'rgb(220, 53, 69)',
                  size = 12,
                  symbol = 'triangle-up',
                  line = list(color = 'white', width = 1)
                ),
                hovertemplate = paste(
                  "<b>MS2扫描 #%{text}</b><br>",
                  "RT: %{x:.2f} s<br>",
                  "强度: %{y:.2e}",
                  "<extra></extra>"
                ),
                text = ~scanNum
              ) %>%
              layout(
                title = list(text = "提取离子色谱图 (EIC)", font = list(size = 16)),
                xaxis = list(
                  title = '保留时间 (秒)',
                  tickfont = list(size = 12),
                  gridcolor = 'rgba(200, 200, 200, 0.3)',
                  zeroline = TRUE,
                  zerolinecolor = 'rgba(150, 150, 150, 0.5)',
                  zerolinewidth = 1
                ),
                yaxis = list(
                  title = 'EIC强度',
                  tickfont = list(size = 12),
                  gridcolor = 'rgba(200, 200, 200, 0.3)',
                  zeroline = TRUE,
                  zerolinecolor = 'rgba(150, 150, 150, 0.5)',
                  zerolinewidth = 1,
                  range = c(0, y_max)  # 从0开始，使用线性坐标
                ),
                hovermode = 'closest',
                margin = list(l = 60, r = 40, t = 60, b = 60),
                plot_bgcolor = 'rgba(248, 249, 250, 1)',
                showlegend = TRUE,
                legend = list(
                  orientation = "h",
                  yanchor = "bottom",
                  y = 1.02,
                  xanchor = "right",
                  x = 1
                )
              )
          }
        })
        
        # 更新MS2谱图
        output$modal_ms2_plot <- renderPlotly({
          if (!is.null(spec$spectrum)) {
            spectrum_df <- data.frame(
              mz = spec$spectrum[, 1],
              intensity = spec$spectrum[, 2],
              relative_intensity = 100 * spec$spectrum[, 2] / max(spec$spectrum[, 2])
            )
            
            # 显示前30个峰
            top_n <- min(30, nrow(spectrum_df))
            if (nrow(spectrum_df) > top_n) {
              spectrum_df <- spectrum_df[order(spectrum_df$intensity, decreasing = TRUE)[1:top_n], ]
              spectrum_df <- spectrum_df[order(spectrum_df$mz), ]
            }
            
            # 为前10个峰添加标签
            top_peaks <- spectrum_df[order(spectrum_df$intensity, decreasing = TRUE)[1:min(10, nrow(spectrum_df))], ]
            
            plot_ly() %>%
              add_segments(
                data = spectrum_df,
                x = ~mz,
                xend = ~mz,
                y = 0,
                yend = ~relative_intensity,
                name = '碎片离子',
                line = list(color = 'rgb(30, 144, 255)', width = 2),
                hovertemplate = paste(
                  "<b>碎片离子</b><br>",
                  "m/z: %{x:.4f}<br>",
                  "相对强度: %{y:.1f}%<br>",
                  "绝对强度: %{text:.2e}",
                  "<extra></extra>"
                ),
                text = ~intensity
              ) %>%
              add_annotations(
                data = top_peaks,
                x = ~mz,
                y = ~relative_intensity,
                text = ~sprintf('%.4f', mz),
                showarrow = TRUE,
                arrowhead = 1,
                arrowsize = 1,
                arrowwidth = 1.5,
                arrowcolor = 'rgb(178, 34, 34)',
                ax = 0,
                ay = -40,
                font = list(size = 11, color = 'rgb(178, 34, 34)', family = 'Arial')
              ) %>%
              layout(
                title = list(text = "MS2质谱图", font = list(size = 16)),
                xaxis = list(
                  title = 'm/z',
                  tickfont = list(size = 12),
                  gridcolor = 'rgba(200, 200, 200, 0.3)',
                  zeroline = TRUE,
                  zerolinecolor = 'rgba(150, 150, 150, 0.5)',
                  zerolinewidth = 1
                ),
                yaxis = list(
                  title = '相对强度 (%)',
                  tickfont = list(size = 12),
                  gridcolor = 'rgba(200, 200, 200, 0.3)',
                  zeroline = TRUE,
                  zerolinecolor = 'rgba(150, 150, 150, 0.5)',
                  zerolinewidth = 1,
                  range = c(0, 105)  # 从0到105%，留一些余量
                ),
                hovermode = 'closest',
                showlegend = FALSE,
                margin = list(l = 60, r = 40, t = 60, b = 60),
                plot_bgcolor = 'rgba(248, 249, 250, 1)'
              )
          }
        })
        
        # 使用JavaScript打开弹窗
        shinyjs::runjs("showVisualizationModal();")
      }
    }
  })
  
  # ==================== 下载功能 ====================
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("ms2_database_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(values$database)
      
      csv_data <- values$database$match_results
      
      mz_columns <- sapply(values$database$spectra, function(x) {
        if (!is.null(x$spectrum)) {
          paste(sprintf("%.4f", x$spectrum[, 1]), collapse = " ")
        } else {
          ""
        }
      })
      
      intensity_columns <- sapply(values$database$spectra, function(x) {
        if (!is.null(x$spectrum)) {
          paste(sprintf("%.0f", x$spectrum[, 2]), collapse = " ")
        } else {
          ""
        }
      })
      
      csv_data$ms2_mz <- mz_columns
      csv_data$ms2_intensity <- intensity_columns
      
      write.csv(csv_data, file, row.names = FALSE)
    }
  )
  
  output$download_rds <- downloadHandler(
    filename = function() {
      paste0("ms2_database_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
    },
    content = function(file) {
      req(values$database)
      saveRDS(values$database, file)
    }
  )
  
  output$download_msp <- downloadHandler(
    filename = function() {
      paste0("ms2_database_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".msp")
    },
    content = function(file) {
      req(values$database)
      
      msp_con <- file(file, "w")
      
      for (spec in values$database$spectra) {
        if (!is.null(spec$spectrum)) {
          writeLines(sprintf("Name: %s", spec$compound_name), msp_con)
          writeLines(sprintf("MW: %.4f", spec$compound_mz), msp_con)
          writeLines(sprintf("RT: %.2f", spec$compound_rt), msp_con)
          writeLines(sprintf("PrecursorMZ: %.4f", spec$precursor_mz), msp_con)
          writeLines(sprintf("PrecursorIntensity: %.0f", spec$precursor_intensity), msp_con)
          writeLines(sprintf("ScanNum: %d", spec$scan_num), msp_con)
          writeLines(sprintf("Num Peaks: %d", nrow(spec$spectrum)), msp_con)
          
          for (j in 1:nrow(spec$spectrum)) {
            writeLines(sprintf("%.4f\t%.0f", spec$spectrum[j, 1], spec$spectrum[j, 2]), msp_con)
          }
          
          writeLines("", msp_con)
        }
      }
      
      close(msp_con)
    }
  )
  
  output$download_combined <- downloadHandler(
    filename = function() {
      req(values$selected_spec)
      paste0(gsub("[^[:alnum:]]", "_", values$selected_spec$compound_name), 
             "_spectra_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(values$selected_spec)
      
      spec <- values$selected_spec
      
      # 创建数据框
      result_data <- data.frame(
        Compound = spec$compound_name,
        Precursor_mz = spec$precursor_mz,
        Precursor_intensity = spec$precursor_intensity,
        Retention_time = spec$retention_time,
        Scan_number = spec$scan_num,
        Match_quality = spec$match_quality,
        Match_score = spec$match_score
      )
      
      # 如果有谱图数据，添加
      if (!is.null(spec$spectrum)) {
        spectrum_data <- data.frame(
          mz = spec$spectrum[, 1],
          intensity = spec$spectrum[, 2],
          relative_intensity = 100 * spec$spectrum[, 2] / max(spec$spectrum[, 2])
        )
        
        # 合并数据
        all_data <- rbind(
          data.frame(Parameter = names(result_data), Value = t(result_data)[1,]),
          data.frame(Parameter = "---", Value = "---"),
          data.frame(Parameter = "Spectrum_Data", Value = "---"),
          spectrum_data
        )
      } else {
        all_data <- data.frame(Parameter = names(result_data), Value = t(result_data)[1,])
      }
      
      write.csv(all_data, file, row.names = FALSE)
    }
  )
}

# ==================== 运行Shiny应用 ====================

shinyApp(ui = ui, server = server)
