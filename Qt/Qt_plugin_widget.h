/********************************************************************************
** Form generated from reading UI file 'qt4_plugin_widget.ui'
**
** Created: Mon Nov 5 18:11:03 2012
**      by: Qt User Interface Compiler version 4.8.1
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_QT4_PLUGIN_WIDGET_H
#define UI_QT4_PLUGIN_WIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_Plugin_widget
{
public:
    QVBoxLayout *vertical_layout;
    QHBoxLayout *horizontalLayout_2;
    QToolButton *info_button;
    QLabel *plugin_info;
    QSpacerItem *horizontalSpacer;
    QToolButton *limiter_bypass_button;
    QToolButton *load_button;
    QToolButton *save_button;
    QToolButton *reset_button;
    QComboBox *interpolation_type;
    QFrame *line_10;

    void setupUi(QWidget *Plugin_widget)
    {
        if (Plugin_widget->objectName().isEmpty())
            Plugin_widget->setObjectName(QString::fromUtf8("Plugin_widget"));
        Plugin_widget->resize(507, 42);
        vertical_layout = new QVBoxLayout(Plugin_widget);
        vertical_layout->setSpacing(0);
        vertical_layout->setContentsMargins(0, 0, 0, 0);
        vertical_layout->setObjectName(QString::fromUtf8("vertical_layout"));
        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(2);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalLayout_2->setContentsMargins(0, -1, 0, -1);
        info_button = new QToolButton(Plugin_widget);
        info_button->setObjectName(QString::fromUtf8("info_button"));

        horizontalLayout_2->addWidget(info_button);

        plugin_info = new QLabel(Plugin_widget);
        plugin_info->setObjectName(QString::fromUtf8("plugin_info"));
        QSizePolicy sizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(plugin_info->sizePolicy().hasHeightForWidth());
        plugin_info->setSizePolicy(sizePolicy);

        horizontalLayout_2->addWidget(plugin_info);

        horizontalSpacer = new QSpacerItem(2, 2, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer);

        limiter_bypass_button = new QToolButton(Plugin_widget);
        limiter_bypass_button->setObjectName(QString::fromUtf8("limiter_bypass_button"));
        limiter_bypass_button->setCheckable(true);

        horizontalLayout_2->addWidget(limiter_bypass_button);

        load_button = new QToolButton(Plugin_widget);
        load_button->setObjectName(QString::fromUtf8("load_button"));

        horizontalLayout_2->addWidget(load_button);

        save_button = new QToolButton(Plugin_widget);
        save_button->setObjectName(QString::fromUtf8("save_button"));

        horizontalLayout_2->addWidget(save_button);

        reset_button = new QToolButton(Plugin_widget);
        reset_button->setObjectName(QString::fromUtf8("reset_button"));

        horizontalLayout_2->addWidget(reset_button);

        interpolation_type = new QComboBox(Plugin_widget);
        interpolation_type->setObjectName(QString::fromUtf8("interpolation_type"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(interpolation_type->sizePolicy().hasHeightForWidth());
        interpolation_type->setSizePolicy(sizePolicy1);

        horizontalLayout_2->addWidget(interpolation_type);


        vertical_layout->addLayout(horizontalLayout_2);

        line_10 = new QFrame(Plugin_widget);
        line_10->setObjectName(QString::fromUtf8("line_10"));
        QPalette palette;
        QBrush brush(QColor(121, 121, 121, 255));
        brush.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::WindowText, brush);
        QBrush brush1(QColor(174, 174, 174, 255));
        brush1.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::Button, brush1);
        QBrush brush2(QColor(195, 195, 195, 255));
        brush2.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::Light, brush2);
        QBrush brush3(QColor(214, 214, 214, 255));
        brush3.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::Midlight, brush3);
        QBrush brush4(QColor(87, 87, 87, 255));
        brush4.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::Dark, brush4);
        QBrush brush5(QColor(116, 116, 116, 255));
        brush5.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::Mid, brush5);
        QBrush brush6(QColor(84, 84, 84, 255));
        brush6.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::Text, brush6);
        QBrush brush7(QColor(205, 205, 205, 255));
        brush7.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::BrightText, brush7);
        QBrush brush8(QColor(117, 117, 117, 255));
        brush8.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::ButtonText, brush8);
        QBrush brush9(QColor(226, 226, 226, 255));
        brush9.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::Base, brush9);
        palette.setBrush(QPalette::Active, QPalette::Window, brush1);
        palette.setBrush(QPalette::Active, QPalette::Shadow, brush6);
        QBrush brush10(QColor(213, 213, 213, 255));
        brush10.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::HighlightedText, brush10);
        palette.setBrush(QPalette::Active, QPalette::AlternateBase, brush3);
        QBrush brush11(QColor(113, 113, 113, 255));
        brush11.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::NoRole, brush11);
        QBrush brush12(QColor(212, 212, 183, 255));
        brush12.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::ToolTipBase, brush12);
        QBrush brush13(QColor(92, 92, 92, 255));
        brush13.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::ToolTipText, brush13);
        palette.setBrush(QPalette::Inactive, QPalette::WindowText, brush);
        palette.setBrush(QPalette::Inactive, QPalette::Button, brush1);
        palette.setBrush(QPalette::Inactive, QPalette::Light, brush2);
        palette.setBrush(QPalette::Inactive, QPalette::Midlight, brush3);
        palette.setBrush(QPalette::Inactive, QPalette::Dark, brush4);
        palette.setBrush(QPalette::Inactive, QPalette::Mid, brush5);
        palette.setBrush(QPalette::Inactive, QPalette::Text, brush6);
        palette.setBrush(QPalette::Inactive, QPalette::BrightText, brush7);
        palette.setBrush(QPalette::Inactive, QPalette::ButtonText, brush8);
        palette.setBrush(QPalette::Inactive, QPalette::Base, brush9);
        palette.setBrush(QPalette::Inactive, QPalette::Window, brush1);
        palette.setBrush(QPalette::Inactive, QPalette::Shadow, brush6);
        palette.setBrush(QPalette::Inactive, QPalette::HighlightedText, brush10);
        palette.setBrush(QPalette::Inactive, QPalette::AlternateBase, brush3);
        palette.setBrush(QPalette::Inactive, QPalette::NoRole, brush11);
        palette.setBrush(QPalette::Inactive, QPalette::ToolTipBase, brush12);
        palette.setBrush(QPalette::Inactive, QPalette::ToolTipText, brush13);
        palette.setBrush(QPalette::Disabled, QPalette::WindowText, brush4);
        palette.setBrush(QPalette::Disabled, QPalette::Button, brush1);
        palette.setBrush(QPalette::Disabled, QPalette::Light, brush2);
        palette.setBrush(QPalette::Disabled, QPalette::Midlight, brush3);
        palette.setBrush(QPalette::Disabled, QPalette::Dark, brush4);
        palette.setBrush(QPalette::Disabled, QPalette::Mid, brush5);
        palette.setBrush(QPalette::Disabled, QPalette::Text, brush4);
        palette.setBrush(QPalette::Disabled, QPalette::BrightText, brush7);
        palette.setBrush(QPalette::Disabled, QPalette::ButtonText, brush4);
        palette.setBrush(QPalette::Disabled, QPalette::Base, brush1);
        palette.setBrush(QPalette::Disabled, QPalette::Window, brush1);
        palette.setBrush(QPalette::Disabled, QPalette::Shadow, brush6);
        palette.setBrush(QPalette::Disabled, QPalette::HighlightedText, brush10);
        palette.setBrush(QPalette::Disabled, QPalette::AlternateBase, brush1);
        palette.setBrush(QPalette::Disabled, QPalette::NoRole, brush11);
        palette.setBrush(QPalette::Disabled, QPalette::ToolTipBase, brush12);
        palette.setBrush(QPalette::Disabled, QPalette::ToolTipText, brush13);
        line_10->setPalette(palette);
        line_10->setFrameShadow(QFrame::Plain);
        line_10->setLineWidth(1);
        line_10->setMidLineWidth(0);
        line_10->setFrameShape(QFrame::HLine);

        vertical_layout->addWidget(line_10);


        retranslateUi(Plugin_widget);

        interpolation_type->setCurrentIndex(2);


        QMetaObject::connectSlotsByName(Plugin_widget);
    } // setupUi

    void retranslateUi(QWidget *Plugin_widget)
    {
        Plugin_widget->setWindowTitle(QApplication::translate("Plugin_widget", "Form", 0, QApplication::UnicodeUTF8));
        info_button->setText(QApplication::translate("Plugin_widget", "?", 0, QApplication::UnicodeUTF8));
        plugin_info->setText(QApplication::translate("Plugin_widget", "Plugin Info", 0, QApplication::UnicodeUTF8));
        limiter_bypass_button->setText(QApplication::translate("Plugin_widget", "Limiter Bypass", 0, QApplication::UnicodeUTF8));
        load_button->setText(QApplication::translate("Plugin_widget", "Load", 0, QApplication::UnicodeUTF8));
        save_button->setText(QApplication::translate("Plugin_widget", "Save", 0, QApplication::UnicodeUTF8));
        reset_button->setText(QApplication::translate("Plugin_widget", "Reset", 0, QApplication::UnicodeUTF8));
        interpolation_type->clear();
        interpolation_type->insertItems(0, QStringList()
         << QApplication::translate("Plugin_widget", "None", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("Plugin_widget", "Linear", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("Plugin_widget", "Cubic", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("Plugin_widget", "Sinc1", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("Plugin_widget", "Sinc2", 0, QApplication::UnicodeUTF8)
        );
#ifndef QT_NO_TOOLTIP
        interpolation_type->setToolTip(QString());
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_STATUSTIP
        interpolation_type->setStatusTip(QString());
#endif // QT_NO_STATUSTIP
#ifndef QT_NO_WHATSTHIS
        interpolation_type->setWhatsThis(QString());
#endif // QT_NO_WHATSTHIS
    } // retranslateUi

};

namespace Ui {
    class Plugin_widget: public Ui_Plugin_widget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_QT4_PLUGIN_WIDGET_H
