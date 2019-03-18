import React             from 'react';
import Attribute         from '../../../components/Attribute';
import styles            from './TagPanel.module.css';


interface Props {
    tags?: string[];
}

export default {
    name:    'tags',
    icon:    'tag',
    tooltip: 'Tag List',
    panel:   (props: Props) =>
        <div>
            <h1>Taglist</h1>
            <div className={styles.TagList}>
                {
                    !props.tags ? null : props.tags.map(tag => (
                        <Attribute key={tag} icon="tag" name={tag} value="1" />
                    ))
                }
            </div>
        </div>
};
